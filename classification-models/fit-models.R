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
library(glmnet)
library(reticulate)

reticulate::use_virtualenv("uea-ucr-2026", required = TRUE)
sklearn_lr <- reticulate::import("sklearn.linear_model")
reticulate::import("warnings")$filterwarnings("ignore")

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
#' @param model_type \code{character} denoting the type of model to fit
#' @param N \code{integer} denoting the number of resamples to compute. Defaults to \code{1} for the canonical train-test split
#' @param seed \code{integer} denoting the fix for pseudorandom reproducibility
#' @return \code{data.frame} containing classification accuracy values for each feature set
#' @author Trent Henderson
#'

fit_models <- function(problem, model_type = c("svm", "xgboost", "glmnet", "pyridge"), N = 1, seed = 123){

  cat(paste0("Evaluating: ", problem, "\n"))

  set.seed(seed)
  model_type <- match.arg(model_type)

  if(paste0(problem, ".csv") %in% list.files(paste0("classification-models/results/", model_type))){ # Prevent unnecessary re-runs
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

    #-----------------------------------------------------------------------------------
    # Decide CV vs fixed-penalty methodology once per problem (used only by pyridge).
    # Each resample preserves the canonical per-class train counts, so this is constant
    # across resamples and feature sets, guaranteeing all feature sets in a problem use
    # the same methodology. CV needs every class to have >= n_folds training samples
    # (StratifiedKFold requirement); otherwise fall back to a fixed C = 1 fit (no CV).
    #-----------------------------------------------------------------------------------

    n_folds <- 10L
    use_cv <- min(train_counts$n) >= n_folds
    penalty_type <- if(model_type == "pyridge") (if(use_cv) "cv" else "fixed") else NA_character_

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

        # Screen on train only: treat non-finite values as missing, then keep features with at
        # least 90% observed values and some variation. Remaining NAs are imputed to 0 below.

        x_train[!is.finite(x_train)] <- NA
        min_prop <- 0.9
        keep <- apply(x_train, 2, function(x){
          obs <- x[!is.na(x)]
          (length(obs) / length(x)) >= min_prop && length(obs) > 1L && var(obs) > 0
        })
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

          # XGBoost handles NAs natively but SVM, glmnet and pyridge don't, so impute remaining NAs (train and test) with 0 (equivalent to the train mean after z-scoring)

          if(model_type %in% c("svm", "glmnet", "pyridge")){
            x_train[is.na(x_train)] <- 0
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
                  num_class = num_class
                ),
                data  = dtrain,
                nrounds = 100,
                verbose = 0
              )

              mean(predict(final_mod, dtest) == y_enc_test)
            }, error = function(e) NA)

          #--------------------
          # Logistic regression
          #--------------------

          } else if(model_type == "pyridge"){
            
            # Use CV-tuned C when the smallest class has enough samples for k-fold (decided once per problem, above),
            # and use fixed C = 1 so small sample size problems still fit
            
            acc <- tryCatch({
              if(use_cv){
                clf <- sklearn_lr$LogisticRegressionCV(
                  Cs = 10L,
                  cv = n_folds,
                  penalty = "l2",
                  solver = "lbfgs",
                  scoring = "accuracy",
                  max_iter = 100L,
                  fit_intercept = TRUE,
                  refit = TRUE,
                  n_jobs = 1L,
                  random_state = 123L
                )
              } else {
                clf <- sklearn_lr$LogisticRegression(
                  C = 1.0,
                  penalty = "l2",
                  solver = "lbfgs",
                  max_iter = 100L,
                  fit_intercept = TRUE,
                  n_jobs = 1L,
                  random_state = 123L
                )
              }
              clf$fit(x_train, as.character(y_train))
              mean(as.character(clf$predict(x_test)) == as.character(y_test))
            }, error = function(e) NA)
            
          }  else if(model_type == "glmnet"){
            
            acc <- tryCatch({
              cv_mod <- glmnet::cv.glmnet(
                x = x_train,
                y = y_train,
                family = "multinomial",
                alpha = 0,
                standardize = FALSE,
                nfolds = 5
              )
              y_pred <- predict(cv_mod, newx = x_test, s = cv_mod$lambda.min, type = "class")
              mean(as.character(y_pred) == as.character(y_test))
            }, error = function(e) NA)
            
          } else{
            
            #------------------------------------------------------------------
            # Linear SVM: no tuning; default cost C = 1 (e1071 C-classification
            # with linear kernel; scale = FALSE as data is already z-scored)
            #------------------------------------------------------------------

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
    results$penalty <- penalty_type

    write.csv(results, paste0("classification-models/results/", model_type, "/", problem, ".csv"), row.names = FALSE)
  }
}

# Run the classifiers

problems <- gsub(".Rda", "\\1", list.files("feature-calculations/features"))
problems <- problems[!problems %in% c("Fungi")]

problems |>
  purrr::walk(~fit_models(problem = .x, model_type = "pyridge", N = 30, seed = 123))

problems |>
  purrr::walk(~fit_models(problem = .x, model_type = "glmnet", N = 30, seed = 123))

problems |>
  purrr::walk(~fit_models(problem = .x, model_type = "pyLR", N = 30, seed = 123))

problems |>
  purrr::walk(~fit_models(problem = .x, model_type = "xgboost", N = 30, seed = 123))

problems |>
  purrr::walk(~fit_models(problem = .x, model_type = "svm", N = 30, seed = 123))
