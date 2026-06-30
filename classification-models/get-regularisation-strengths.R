#---------------------------------------
# This script refits the pyridge
# (LogisticRegressionCV) classifier for
# each feature set on each problem and,
# instead of storing accuracy, records
# the regularisation strength (C) that
# was selected by cross-validation
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 30 June 2026
#---------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
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

#' Function that can iterate over problems and return the CV-selected regularisation strengths
#'
#' Mirrors the data loading, screening and normalisation pipeline of fit-models.R exactly
#' (same seed, same number of resamples) so that the regularisation strengths recorded here
#' correspond to the same fits that produced the pyridge accuracy results. Rather than scoring
#' the model, it extracts the inverse-regularisation-strength parameter C that
#' LogisticRegressionCV selected via k-fold cross-validation (the \code{C_} attribute).
#'
#' @param problem \code{character} denoting the dataset to work on
#' @param N \code{integer} denoting the number of resamples to compute. Defaults to \code{30}
#' @param seed \code{integer} denoting the fix for pseudorandom reproducibility
#' @return \code{data.frame} of feature_set, resample, problem and the selected regularisation strength C
#' @author Trent Henderson
#'

fit_regularisation <- function(problem, N = 30, seed = 123){
  
  cat(paste0("Evaluating: ", problem, "\n"))
  
  set.seed(seed)
  
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
  # CV is only meaningful when every class has >= n_folds training samples. The driver
  # below restricts this script to problems that used CV (penalty == "cv") in the
  # pyridge accuracy run, so this should always be TRUE here; we keep the guard so any
  # problem that would have fallen back to a fixed C is skipped rather than mislabelled.
  #-----------------------------------------------------------------------------------
  
  n_folds <- 10L
  use_cv <- min(train_counts$n) >= n_folds
  
  if(!use_cv){
    message(paste0("Skipping ", problem, ": did not use CV (fixed-penalty fallback)"))
    return(NULL)
  }
  
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
        tmp <- data.frame(feature_set = i, resample = r, C = NA_real_)
        results[[counter]] <- tmp
        counter <- counter + 1
      } else{
        
        # Normalise data matrices using values from train data to keep test unseen
        
        rescalers <- get_rescale_vals(x_train)
        x_train <- rescale_zscore(x_train, rescalers)
        x_test <- rescale_zscore(x_test,  rescalers)
        
        # pyridge doesn't handle NAs, so impute remaining NAs (train and test) with 0 (equivalent to the train mean after z-scoring)
        
        x_train[is.na(x_train)] <- 0
        x_test[is.na(x_test)] <- 0
        
        #--------------------------------------------------------------------------------------
        # Logistic regression: extract the C (inverse regularisation strength) chosen by CV.
        # LogisticRegressionCV stores this in C_ (one entry per class; identical across classes
        # for the multinomial fit produced by the lbfgs solver), so we take the first element.
        #--------------------------------------------------------------------------------------
        
        C_val <- tryCatch({
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
          clf$fit(x_train, as.character(y_train))
          as.numeric(clf$C_)[1]
        }, error = function(e) NA_real_)
        
        tmp <- data.frame(
          feature_set = i,
          resample = r,
          C = C_val
        )
        
        results[[counter]] <- tmp
        counter <- counter + 1
      }
    }
  }
  
  # Bind results together
  
  results <- do.call(rbind, results)
  results$problem <- problem
  results <- results[, c("feature_set", "resample", "problem", "C")]
  return(results)
}

#--------------- Determine which problems to run ---------------

# Restrict to the problems already computed for the pyridge model that used CV
# (penalty == "cv"), then drop the two problems we are ignoring

model_type <- "pyridge"

files <- list.files(paste0("classification-models/results/", model_type, "/"))
accuracies <- vector(mode = "list", length = length(files))

for(i in files){
  feature_sets <- read.csv(paste0("classification-models/results/", model_type, "/", i))
  accuracies[[match(i, files)]] <- feature_sets
}

accuracies <- do.call("rbind", accuracies) |> filter(penalty == "cv")

cv_problems <- accuracies |>
  dplyr::filter(!problem %in% c("Crop", "ElectricDevices")) |>
  dplyr::pull(problem) |>
  unique()

#--------------- Run the function ---------------

regularisation_strengths <- cv_problems |>
  purrr::map_dfr(~fit_regularisation(problem = .x, N = 30, seed = 123))

write.csv(regularisation_strengths, "classification-models/regularisation_strengths.csv")

# Compute summaries

regularisation_strengths |>
  reframe(.mean = mean(C),
          .median = median(C),
          .sd = sd(C),
          .min = min(C),
          .max = max(C),
          .by = "feature_set")
