#------------------------------------------
# This script sets out to compute 
# classification accuracy for each feature 
# set by problem
#
# NOTE: This script requires setup.R and
# analysis/compute-features.R to have been 
# run first
#-----------------------------------------

#------------------------------------
# Author: Trent Henderson, 5 May 2022
#------------------------------------

# Load feature data

load("data/FeatureMatrix.Rda")

#---------------- Classification accuracy -----------------

#' Function to map classification performance calculations over datasets/problems
#' @param data the dataset containing all raw time series
#' @param theproblem string specifying the problem to calculate features for
#' @returns an object of class list
#' @author Trent Henderson
#' 

calculate_accuracy_by_problem <- function(data, theproblem){
  
  message("Doing problem ", match(theproblem, unique(data$problem)), "/", length(unique(data$problem)))
  
  tmp <- data %>%
    filter(problem == theproblem)
  
  # Fit multi-feature classifiers by feature set
  
  results <- fit_multi_feature_classifier(tmp, 
                                          id_var = "id", 
                                          group_var = "group",
                                          by_set = TRUE, 
                                          test_method = "svmLinear", 
                                          use_balanced_accuracy = TRUE,
                                          use_k_fold = TRUE, 
                                          num_folds = 10, 
                                          use_empirical_null = TRUE, 
                                          null_testing_method = "model free shuffles",
                                          p_value_method = "gaussian", 
                                          num_permutations = 1000, 
                                          seed = 123)
  
  return(results)
}

# Run function

calculate_accuracy_by_problem_safe <- purrr::possibly(calculate_accuracy_by_problem, otherwise = NULL)

outputs <- unique(FeatureMatrix$problem) %>%
  purrr::map(~ calculate_accuracy_by_problem_safe(data = FeatureMatrix, theproblem = .x))

# Name list entries for easier viewing

names(outputs) <- unique(FeatureMatrix$problem)
