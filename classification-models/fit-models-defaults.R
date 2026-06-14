#---------------------------------------
# This script fits classifiers for each
# feature set on each problem using the
# Probst et al. (2019) optimal defaults
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 10 June 2026
#---------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(xgboost)
library(e1071)

#--------------- Define functions ---------------

#' Calculate central tendency and spread values for all numeric columns in a dataset
#'
#' @param data \code{matrix} containing data to normalise
#' @return \code{list} of central tendency and spread values
#' @author Trent Henderson
#'

get_rescale_vals <- function(data){
  ct <- colMeans(data, na.rm = TRUE)
  spreads <- apply(data, 2, sd, na.rm = TRUE)
  outs <- list(ct, spreads)
  names(outs) <- c("CentralTendency", "Spread")
  return(outs)
}

#' Calculate z-score for all columns in a dataset using train set central tendency and spread
#'
#' @param data \code{matrix} containing data to normalise
#' @param rescalers \code{list} containing central tendency and spread values for the train set
#' @return \code{matrix} of rescaled data
#' @author Trent Henderson
#'

rescale_zscore <- function(data, rescalers){
  sweep(
    sweep(data, 2, rescalers$CentralTendency, "-"), 2, rescalers$Spread, "/"
    )
}

#' Helper function to generate IDs for resamples
#'
#' @param features \code{feature_calculations} object containing feature data
#' @param train_counts \code{data.frame} denoting number of train samples
#' @param test_counts \code{data.frame} denoting number of test samples
#' @return \code{list} of train-test IDs
#' @author Trent Henderson
#'

generate_resample_indx <- function(features, train_counts, test_counts){

  all_ids <- features |> dplyr::distinct(id, group)
  train_ids <- c()
  test_ids <- c()

  for(g in unique(all_ids$group)){

    ids_g <- all_ids |>
      dplyr::filter(group == g) |>
      dplyr::pull(id)

    n_train_g <- train_counts |>
      dplyr::filter(group == g) |>
      dplyr::pull(n)

    n_test_g <- test_counts |>
      dplyr::filter(group == g) |>
      dplyr::pull(n)

    total_needed <- n_train_g + n_test_g
    sampled <- sample(ids_g, size = total_needed, replace = FALSE)
    train_ids <- c(train_ids, sampled[seq_len(n_train_g)])
    test_ids <- c(test_ids, sampled[(n_train_g + 1):total_needed])
  }

  return(list(train_ids = train_ids, test_ids = test_ids))
}

#' Function that can iterate over problems and save outputs
#'
#' @param problem \code{character} denoting the dataset to work on
#' @param model_type \code{character} denoting the type of model to fit. Can be one of \code{"svm"} or \code{"xgboost"}
#' @param N \code{integer} denoting the number of resamples to compute. Defaults to \code{1} for the canonical train-test split
#' @param seed \code{integer} denoting the fix for pseudorandom reproducibility
#' @return \code{data.frame} containing classification accuracy values for each feature set
#' @author Trent Henderson
#'

fit_models_defaults <- function(problem, model_type = c("svm", "xgboost"), N = 1, seed = 123){

  cat(paste0("Evaluating: ", problem, "\n"))

  set.seed(seed)
  model_type <- match.arg(model_type)

  if(paste0(problem, ".csv") %in% list.files(paste0("classification-models/results-defaults/", model_type))){ # Prevent unnecessary re-runs
    return(NA)
  } else{

    #----------
    # Load data
    #----------

    suppressWarnings(rm(features, label, all_baseline))
    load(paste0("feature-calculations/features/", problem, ".Rda"))
    load(paste0("feature-calculations/baseline-features/", problem, ".Rda"))
    load(paste0("feature-calculations/train-test-labels/", problem, ".Rda"))

    features <- bind_rows(features, all_baseline)
    feature_sets <- unique(features$feature_set)

    label <- label |>
      dplyr::select(-c(problem)) |>
      dplyr::distinct() # Fixed upstream in code in feature-calculations/calculate-features.R but I had already run everything...

    features <- features |>
      dplyr::inner_join(label, by = c("id" = "id"))

    # Parse into canonical train-test split

    features_train <- features |> dplyr::filter(train_test == "Train")
    features_test <- features |> dplyr::filter(train_test == "Test")

    #--------------------------
    # Compute class proportions
    #--------------------------

    train_counts <- features_train |> dplyr::distinct(id, group) |> dplyr::count(group)
    test_counts <- features_test  |> dplyr::distinct(id, group) |> dplyr::count(group)

    #--------------
    # Do resampling
    #--------------

    # Pre-pivot all feature sets to wide format once
    
    wide_data <- lapply(feature_sets, function(i){
      features |>
        dplyr::filter(feature_set == i) |>
        tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values")
    })
    names(wide_data) <- feature_sets

    canonical_train_ids <- unique(features_train$id)
    canonical_test_ids <- unique(features_test$id)

    results <- list()
    counter <- 1

    for(r in seq_len(N)){

      message(paste0("Resample: ", r))

      if(r == 1){
        train_ids_r <- canonical_train_ids
        test_ids_r <- canonical_test_ids
      } else{
        idx <- generate_resample_indx(features, train_counts, test_counts)
        train_ids_r <- idx$train_ids
        test_ids_r  <- idx$test_ids
      }

      # Execute main loop over feature sets

      for(i in feature_sets){

        message(paste0("Evaluating feature set: ", i))

        wd <- wide_data[[i]]

        # Train

        x_train_df <- wd |> dplyr::filter(id %in% train_ids_r) |> dplyr::select(-id)
        y_train <- x_train_df$group
        x_train <- x_train_df |> dplyr::select(-group) |> as.matrix()

        # Screen on train only: drop columns with any non-finite values

        keep <- apply(x_train, 2, function(x) all(is.finite(x)) && isTRUE(var(x) > 0))
        x_train <- x_train[, keep, drop = FALSE]

        # Test

        x_test_df <- wd |> dplyr::filter(id %in% test_ids_r) |> dplyr::select(-id)
        y_test <- x_test_df$group
        x_test <- x_test_df |> dplyr::select(-group) |> as.matrix()

        # Align test to train's screened set and treat non-finite test values as missing

        x_test <- x_test[, colnames(x_train), drop = FALSE]
        x_test[!is.finite(x_test)] <- NA

        if(ncol(x_train) == 0){
          tmp <- data.frame(feature_set = i, num_features = 0, accuracy = NA, resample = r)
          results[[counter]] <- tmp
          counter <- counter + 1
        } else{

          # Normalise data matrices using values from train data to keep test unseen

          rescalers <- get_rescale_vals(x_train)
          x_train <- rescale_zscore(x_train, rescalers)
          x_test <- rescale_zscore(x_test,  rescalers)

          # XGBoost handles NAs natively but SVM doesn't, so impute remaining test NAs with 0 (equivalent to the train mean after z-scoring)
          
          if(model_type == "svm"){
            x_test[is.na(x_test)] <- 0
          }

          #---------------------------------------------------------------------------------------------
          # XGBoost: no tuning; all hyperparameters fixed to the opt. defaults from Probst et al. (2019)
          #---------------------------------------------------------------------------------------------

          if(model_type == "xgboost"){

            classes <- sort(unique(y_train))
            y_enc <- as.integer(factor(y_train, levels = classes)) - 1L
            y_enc_test <- as.integer(factor(y_test,  levels = classes)) - 1L
            num_class <- length(classes)

            acc <- tryCatch({
              dtrain <- xgboost::xgb.DMatrix(x_train, label = y_enc)
              dtest <- xgboost::xgb.DMatrix(x_test,  label = y_enc_test)

              final_mod <- xgboost::xgb.train(
                params = list(
                  objective = "multi:softmax",
                  num_class = num_class,
                  eval_metric = "merror",
                  tree_method = "hist",
                  eta = 0.018,
                  subsample = 0.839,
                  max_depth = 13L,
                  min_child_weight  = 2.06,
                  colsample_bytree  = 0.752,
                  colsample_bylevel = 0.585,
                  lambda = 0.982,
                  alpha = 1.113,
                  nthread = 1
                ),
                data  = dtrain,
                nrounds = 4168,
                verbose = 0
              )

              mean(predict(final_mod, dtest) == y_enc_test)
            }, error = function(e) NA)

          #------------------------------------------------------------------
          # Linear SVM: no tuning; default cost C = 1 (e1071 C-classification
          # with linear kernel; scale = FALSE as data is already z-scored)
          #------------------------------------------------------------------

          } else{

            acc <- tryCatch({
              final_mod <- e1071::svm(
                x = x_train,
                y = factor(y_train),
                kernel = "linear",
                cost = 1,
                scale  = FALSE
              )
              mean(as.character(predict(final_mod, x_test)) == y_test)
            }, error = function(e) NA)
          }

          tmp <- data.frame(
            feature_set = i,
            num_features = ncol(x_train),
            accuracy = acc,
            resample = r
          )

          results[[counter]] <- tmp
          counter <- counter + 1
        }
      }
    }

    # Bind results together

    results <- do.call(rbind, results)
    results$problem <- problem
    results$classifier <- model_type

    write.csv(results, paste0("classification-models/results/", model_type, "/", problem, ".csv"), row.names = FALSE)
  }
}

# Run the classifiers

problems <- gsub(".Rda", "\\1", list.files("feature-calculations/features"))

problems |>
  purrr::walk(~fit_models_defaults(problem = .x, model_type = "xgboost", N = 30, seed = 123))

problems |>
  purrr::walk(~fit_models_defaults(problem = .x, model_type = "svm", N = 30, seed = 123))
