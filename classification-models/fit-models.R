#---------------------------------------
# This script fits classifiers for each
# feature set on each problem
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 4 May 2026
#---------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(glmnet)
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
  test_ids  <- c()
  
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
#' @param model_type \code{character} denoting the type of model to fit. Can be one of \code{"glmnet"}, \code{"svm"}, or \code{"rbfsvm"}
#' @param N \code{integer} denoting the number of resamples to compute. Defaults to \code{1} for the canonical train-test split
#' @param seed \code{integer} denoting the fix for pseudorandom reproducibility
#' @return \code{data.frame} containing classification accuracy values for each feature set
#' @author Trent Henderson
#' 

fit_models <- function(problem, model_type = c("glmnet", "svm", "rbfsvm"), N = 1, seed = 123){

  cat(paste0("Evaluating: ", problem, "\n"))

  set.seed(seed)
  match.arg(model_type)
  stopifnot(model_type %in% c("glmnet", "svm", "rbfsvm"))
  
  if(paste0(problem, ".csv") %in% list.files(paste0("classification-models/results/", model_type))){ # Prevent unnecessary re-runs
    return(NA)
  } else{
    
    #----------
    # Load data
    #----------
    
    rm(features, label)
    load(paste0("feature-calculations/features/", problem, ".Rda"))
    load(paste0("feature-calculations/train-test-labels/", problem, ".Rda"))
    
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
    test_counts <- features_test |> dplyr::distinct(id, group) |> dplyr::count(group)
    
    #--------------
    # Do resampling
    #--------------
    
    results <- list()
    counter <- 1
    
    for(r in seq_len(N)){
      
      message(paste0("Resample: ", r))
      
      if(r == 1){ # Canonical train-test split
        features_train_r <- features_train
        features_test_r <- features_test
      } else{
        idx <- generate_resample_indx(features, train_counts, test_counts)
        features_train_r <- features |> dplyr::filter(id %in% idx$train_ids)
        features_test_r <- features |> dplyr::filter(id %in% idx$test_ids)
      }
      
      # Execute main loop over feature sets
      
      for(i in feature_sets){
        
        message(paste0("Evaluating feature set: ", i))
        
        # Train
        
        x_train <- features_train_r |>
          dplyr::filter(feature_set == i) |>
          tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") |>
          dplyr::select(-c(id))
        
        y_train <- x_train$group
        x_train <- x_train |> dplyr::select(-group) |> as.matrix()
        x_train <- x_train[, colSums(is.na(x_train)) == 0, drop = FALSE] # Remove features that have NA values
        x_train <- x_train[, apply(x_train, 2, function(x) length(unique(x)) > 1), drop = FALSE] # Remove constant columns
        x_train <- x_train[, apply(x_train, 2, var, na.rm = TRUE) != 0, drop = FALSE] # Remove columns where SD = 0
        
        # Test
        
        x_test <- features_test_r |>
          dplyr::filter(feature_set == i) |>
          tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") |>
          dplyr::select(-c(id))
        
        y_test <- x_test$group
        x_test <- x_test |> dplyr::select(-group) |> as.matrix()
        x_test <- x_test[, colnames(x_test) %in% colnames(x_train), drop = FALSE]
        x_test <- x_test[, colSums(is.na(x_test)) == 0, drop = FALSE]
        x_test <- x_test[, apply(x_test, 2, function(x) length(unique(x)) > 1), drop = FALSE]
        x_test <- x_test[, apply(x_test, 2, var, na.rm = TRUE) != 0, drop = FALSE]
        x_train <- x_train[, colnames(x_train) %in% colnames(x_test), drop = FALSE] # Filter to those that met the criteria for valid columns back the other way
        x_test <- x_test[, colnames(x_train), drop = FALSE] # Filter to those that met the criteria for valid columns back the other way -- avoids some errors in column number differences...
        
        if(ncol(x_train) == 0 || ncol(x_test) == 0){
          tmp <- data.frame(feature_set = i, num_features = 0, accuracy = NA, resample = r)
          results[[counter]] <- tmp
          counter <- counter + 1
        } else{
          
          # Normalise data matrices using values from train data to keep test unseen
          
          rescalers <- get_rescale_vals(x_train)
          x_train <- rescale_zscore(x_train, rescalers)
          x_test <- rescale_zscore(x_test, rescalers)
          
          # Fit model and compute accuracy metrics
          
          if(model_type == "glmnet"){
            
            mod <- try(glmnet::cv.glmnet(x_train, y_train, family = "multinomial", alpha = 0, standardize = FALSE))
            
            if(inherits(mod, "try-error")){
              acc <- NA
            } else{
              y_pred <- predict(mod, newx = x_test, s = mod$lambda.min, type = "class")
              cm <- table(y_pred, y_test)
              acc <- sum(diag(cm)) / sum(cm)
            }
            
          } else if(model_type == "rbfsvm"){
            
            train_df <- as.data.frame(x_train)
            train_df$group <- y_train
            
            mod <- try(e1071::svm(group ~ ., data = train_df, kernel = "radial", scale = FALSE, probability = TRUE))
            
            if(inherits(mod, "try-error")){
              acc <- NA
            } else{
              y_pred <- predict(mod, newdata = x_test)
              cm <- table(y_pred, y_test)
              acc <- sum(diag(cm)) / sum(cm)
            }
            
          } else{

            train_df <- as.data.frame(x_train)
            train_df$group <- y_train

            mod <- try(e1071::svm(group ~ ., data = train_df, kernel = "linear", scale = FALSE))

            if(inherits(mod, "try-error")){
              acc <- NA
            } else{
              y_pred <- predict(mod, newdata = x_test)
              cm <- table(y_pred, y_test)
              acc <- sum(diag(cm)) / sum(cm)
            }
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

gsub(".Rda", "\\1", list.files("feature-calculations/features")) |>
  purrr::map_dfr(~fit_models(problem = .x, model_type = "svm", N = 30, seed = 123))

gsub(".Rda", "\\1", list.files("feature-calculations/features")) |>
  purrr::map_dfr(~fit_models(problem = .x, model_type = "rbfsvm", N = 30, seed = 123))
