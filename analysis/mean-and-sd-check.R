#------------------------------------------
# This script sets out to compute features
# for mean and SD to understand which
# problems are appropriately z-scored to
# retain
#
# NOTE: This script requires setup.R and
# analysis/prepare-time-series-data.R 
# to have been run first
#-----------------------------------------

#-------------------------------------
# Author: Trent Henderson, 7 June 2022
#-------------------------------------

# Load data

load("data/TimeSeriesData.Rda")

#------------- Feature extraction --------------

#' Function to map over datasets to avoid massive dataframe processing times / crashes
#' @param data the dataset containing all raw time series
#' @param theproblem string specifying the problem to calculate features for
#' @returns an object of class dataframe
#' @author Trent Henderson
#' 

extract_mean_and_sd <- function(data, theproblem){
  
  message(paste0("Doing problem ", match(theproblem, unique(data$problem)), "/", length(unique(data$problem))))
  
  # Filter to problem of interest and calculate features
  
  outs <- data %>%
    filter(problem == theproblem) %>%
    dplyr::rename(group = target) %>%
    dplyr::select(c(.data$id, .data$timepoint, .data$values, .data$group, .data$set_split, .data$problem)) %>%
    dplyr::group_by(.data$id, .data$group, .data$set_split, .data$problem) %>%
    dplyr::arrange(.data$timepoint) %>%
    dplyr::summarise(mu = mean(.data$values, na.rm = TRUE),
                     sigma = stats::sd(.data$values, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(method = "Mean and variance") %>%
    tidyr::pivot_longer(cols = mu:sigma, names_to = "names", values_to = "values")
  
  return(outs)
}

# Run the function

extract_mean_and_sd_safe <- purrr::possibly(extract_mean_and_sd, otherwise = NULL)

mean_sd_test <- unique(TimeSeriesData$problem) %>%
  purrr::map_df(~ extract_mean_and_sd_safe(data = TimeSeriesData, theproblem = .x))

rm(TimeSeriesData)

#------------- Classification performance --------------

# NOTE: Goal is to determine is mean and variance can outperform chance to determine effect of z-scoring

#-----------------
# Resamples method
#-----------------

#' Function to map classification performance calculations over datasets/problems
#' @param data the dataframe to operate on
#' @param theproblem filepath to the feature data
#' @returns an object of class dataframe
#' @author Trent Henderson
#' 

calculate_accuracy_for_mean_sd <- function(data, theproblem){
  
  message(paste0("Doing problem ", match(theproblem, unique(data$problem)), "/", length(unique(data$problem))))
          
  outs <- data %>%
    filter(problem == theproblem)
  
  # Fit multi-feature classifiers by feature set
  
  results <- fit_multi_feature_classifier_tt(outs, 
                                             id_var = "id", 
                                             group_var = "group",
                                             by_set = FALSE, 
                                             test_method = "svmLinear", 
                                             use_balanced_accuracy = TRUE,
                                             use_k_fold = TRUE, 
                                             num_folds = 10, 
                                             num_resamples = 10) %>%
    mutate(problem = theproblem)
  
  return(results)
}

calculate_accuracy_for_mean_sd_safe <- purrr::possibly(calculate_accuracy_for_mean_sd, otherwise = NULL)

mean_sd_outputs <- unique(mean_sd_test$problem) %>%
  purrr::map_df(~ calculate_accuracy_for_mean_sd_safe(data = mean_sd_test, theproblem = .x))

save(mean_sd_outputs, file = "data/mean_sd_outputs.Rda")

#---------------------------
# Model-free shuffles method
#---------------------------

#' Function to map classification performance calculations over datasets/problems
#' @param data the dataframe to operate on
#' @param theproblem filepath to the feature data
#' @returns an object of class dataframe
#' @author Trent Henderson
#' 

calculate_accuracy_model_free <- function(data, theproblem){
  
  message(paste0("Doing problem ", match(theproblem, unique(data$problem)), "/", length(unique(data$problem))))
  
  outs <- data %>%
    filter(problem == theproblem)
  
  # Fit multi-feature classifiers by feature set
  
  results <- fit_multi_feature_classifier(outs, 
                                          id_var = "id", 
                                          group_var = "group", 
                                          by_set = FALSE, 
                                          test_method = "svmLinear",
                                          use_balanced_accuracy = TRUE,
                                          use_k_fold = TRUE,
                                          num_folds = 10,
                                          use_empirical_null =  TRUE,
                                          null_testing_method = "model free shuffles",
                                          p_value_method = "gaussian",
                                          num_permutations = 10000,
                                          seed = 123)$TestStatistics %>%
    mutate(problem = theproblem)
  
  return(results)
}

calculate_accuracy_model_free_safe <- purrr::possibly(calculate_accuracy_model_free, otherwise = NULL)

mean_sd_outputs_model_free <- unique(mean_sd_test$problem) %>%
  purrr::map_df(~ calculate_accuracy_model_free_safe(data = mean_sd_test, theproblem = .x))

save(mean_sd_outputs_model_free, file = "data/mean_sd_outputs_model_free.Rda")
