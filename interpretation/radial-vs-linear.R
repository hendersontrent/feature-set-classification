#-------------------------------------
# This script compares the performance
# by feature set between a linear SVM
# and an RBF SVM
#-------------------------------------

#-------------------------------------
# Author: Trent Henderson, 21 May 2026
#-------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(correctR)

#--------------- Define functions ---------------

#' Get train-test sample sizes for each problem
#' 
#' @return \code{data.frame} of problem summaries
#' @author Trent Henderson
#' 

get_n <- function(){
  
  n_storage <- vector(mode = "list", length = length(list.files("feature-calculations/train-test-labels/")))
  
  for(i in 1:length(n_storage)){
    rm(label)
    load(paste0("feature-calculations/train-test-labels/", list.files("feature-calculations/train-test-labels/")[i]))
    
    n_tmp <- label |>
      distinct() |>
      reframe(counter = n(), .by = c("problem", "train_test")) |>
      pivot_wider(id_cols = "problem", names_from = "train_test", values_from = "counter")
    
    n_storage[[i]] <- n_tmp
  }
  
  n_storage <- do.call("rbind", n_storage)
  return(n_storage)
}

#' Pull and reshape classification results
#' 
#' @return \code{data.frame} containing the results
#' @author Trent Henderson
#' 

linear_rbf <- function(){
  
  # Pull linear SVM results
  
  files <- list.files(paste0("classification-models/results/svm"))
  accuracies <- vector(mode = "list", length = length(files))
  
  for(i in files){
    feature_sets <- read.csv(paste0("classification-models/results/svm/", i))
    accuracies[[match(i, files)]] <- feature_sets
  }
  
  accuracies <- do.call("rbind", accuracies) |>
    filter(feature_set != "timegp") |> # From another related project which used the same methodology
    rename(accuracy_linear = accuracy) |>
    dplyr::select(-c(classifier))
  
  # Pull RBF SVM results
  
  files2 <- list.files(paste0("classification-models/results/rbfsvm"))
  accuracies2 <- vector(mode = "list", length = length(files2))
  
  for(j in files2){
    feature_sets2 <- read.csv(paste0("classification-models/results/rbfsvm/", j))
    accuracies2[[match(j, files2)]] <- feature_sets2
  }
  
  accuracies2 <- do.call("rbind", accuracies2) |>
    rename(accuracy_rbf = accuracy) |>
    dplyr::select(-c(classifier))
  
  # Merge and pivot
  
  accuracies3 <- accuracies |>
    inner_join(accuracies2) |>
    mutate(.diff = accuracy_rbf - accuracy_linear)
  
  return(accuracies3)
}
  
#--------------- Do analysis ---------------

results <- linear_rbf()

# Compute average lift due to RBF SVM (i.e., a more complex classifier)

results |>
  reframe(.mean = mean(.diff))

results |>
  reframe(.mean = mean(.diff), .by = "feature_set")
