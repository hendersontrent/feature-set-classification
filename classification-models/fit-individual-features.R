#---------------------------------------
# This script fits a linear SVM for each
# individual time-series feature across
# five UCR problems and returns a data
# frame of per-feature accuracies
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 30 May 2026
#---------------------------------------

library(dplyr)
library(parallel)
library(e1071)

# Set up problems to iterate over

problems <- c("SyntheticControl", "TwoPatterns", "Wine", "Beef", "EthanolLevel")

# Set up parallelisation options

n_cores <- max(1L, parallel::detectCores() - 1L)

#' Fit a linear SVM for a single feature and return accuracy
#'
#' @param feat_key \code{data.frame} row with columns \code{feature_set} and \code{names}
#' @param features \code{data.frame} of long-format feature values
#' @param train_ids \code{character} vector of training time-series IDs
#' @param test_ids \code{character} vector of test time-series IDs
#' @param problem \code{character} problem name, stored in the result
#' @return \code{data.frame} with columns \code{feature_name}, \code{problem}, \code{accuracy}
#' @author Trent Henderson

fit_single_feature <- function(feat_key, features, train_ids, test_ids, problem) {

  feat_name <- paste0(feat_key$feature_set, "_", feat_key$names)

  sub <- features[
    features$feature_set == feat_key$feature_set & features$names == feat_key$names,
    c("id", "group", "values")
  ]

  train_sub <- sub[sub$id %in% train_ids, ]
  test_sub<- sub[sub$id %in% test_ids, ]

  x_train <- train_sub$values
  y_train <- train_sub$group
  x_test <- test_sub$values
  y_test <- test_sub$group

  acc <- tryCatch({

    if(nrow(train_sub) < 2 || nrow(test_sub) < 1 ||
        any(is.na(x_train)) || any(is.na(x_test)) ||
        length(unique(x_train)) <= 1){
      NA
    } else{
      mn <- mean(x_train)
      sd_v <- sd(x_train)

      if(is.na(sd_v) || sd_v == 0){
        return(NA)
      }

      x_tr <- (x_train - mn) / sd_v
      x_te <- (x_test  - mn) / sd_v

      train_df <- data.frame(x = x_tr, group = y_train)
      mod <- e1071::svm(group ~ x, data = train_df, kernel = "linear", scale = FALSE)
      y_pred <- predict(mod, newdata = data.frame(x = x_te))
      cm <- table(y_pred, y_test)
      sum(diag(cm)) / sum(cm)
    }

  }, error = function(e) NA)

  data.frame(
    feature_name = feat_name,
    problem = problem,
    accuracy = acc,
    stringsAsFactors = FALSE
  )
}

#' Evaluate all individual features for one problem
#'
#' @param problem \code{character} problem name
#' @return \code{data.frame} with feature_name, problem, accuracy for every feature
#' @author Trent Henderson

evaluate_problem <- function(problem) {
  message("Doing: ", problem)

  load(paste0("feature-calculations/features/", problem, ".Rda"))
  load(paste0("feature-calculations/train-test-labels/", problem, ".Rda"))

  label <- label |>
    dplyr::select(id, train_test) |>
    dplyr::distinct()

  train_ids <- label$id[label$train_test == "Train"]
  test_ids <- label$id[label$train_test == "Test"]

  feat_keys <- unique(features[, c("feature_set", "names")])
  feat_key_list <- lapply(seq_len(nrow(feat_keys)), function(i) feat_keys[i, , drop = FALSE])

  results <- parallel::mclapply(
    feat_key_list,
    fit_single_feature,
    features = features,
    train_ids = train_ids,
    test_ids = test_ids,
    problem = problem,
    mc.cores = n_cores
  )

  do.call(rbind, results)
}

# Run across all problems and collect results

all_results <- lapply(problems, evaluate_problem) |> 
  do.call(what = "rbind")
