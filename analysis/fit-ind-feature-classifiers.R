#-----------------------------------------
# This script sets out to compute 
# classification accuracy for each feature
# for each problem
#-----------------------------------------

#----------------------------------------
# Author: Trent Henderson, 19 August 2023
#----------------------------------------

load("data/good_keepers.Rda")

# Get train-test split labels

load("data/TimeSeriesData.Rda")

train_test_ids <- TimeSeriesData %>%
  dplyr::select(c(problem, id, set_split)) %>%
  distinct()

rm(TimeSeriesData) # Clean up environment as dataframe is large

#----------------Model fitting -----------------

#' Fit classifiers for all individual features
#' 
#' @param problem_name \code{string} denoting the problem to analyse
#' @param tt_labels \code{data.frame} containing train-test split information
#' @param n_resamples \code{integer} denoting the number of resamples to calculate. Defaults to \code{10}
#' @param z_scored \code{Boolean} whether the feature data is z-scored or not. Defaults to \code{FALSE}
#' @return \code{data.frame} of classification results
#' @author Trent Henderson
#' 

fit_feature_classifiers <- function(problem_name, tt_labels, n_resamples = 10, z_scored = FALSE){
  
  if(z_scored){
    load(paste0("data/feature-calcs/z-scored/", problem_name, ".Rda"))
  } else{
    load(paste0("data/feature-calcs/", problem_name, ".Rda"))
  }
  
  # Get proportion of labels in train set
  
  train_prop <- tt_labels %>%
    filter(problem == problem_name) %>%
    group_by(set_split) %>%
    summarise(counter = n()) %>%
    ungroup() %>%
    mutate(props = counter / sum(counter)) %>%
    filter(set_split == "Train") %>%
    pull(props)
  
  # Set up data for {theft} and run models
  
  outs <- outs %>%
    rename(feature_set = method)
  
  outs <- structure(list(outs), class = "feature_calculations")
  
  outputs <- tsfeature_classifier(outs, 
                                  train_size = train_prop,
                                  by_set = FALSE, 
                                  n_resamples = n_resamples, 
                                  use_null = TRUE)
  
  outputs[[2]]$problem <- problem_name
  
  save(outputs, file = paste0("data/individual-feature-classifiers/", problem_name, ".Rda"))
}

# Run function for all problems on z-scored data

outputs_ind_feature <- good_keepers[64:length(good_keepers)] %>%
  purrr::map(~ fit_feature_classifiers(problem_name = .x, 
                                       tt_labels = train_test_ids,
                                       n_resamples = 10, 
                                       z_scored = TRUE))
