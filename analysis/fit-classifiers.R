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

#---------------- Classification accuracy -----------------

#' Function to map classification performance calculations over datasets/problems
#' @param theproblem filepath to the feature data
#' @returns an object of class list
#' @author Trent Henderson
#' 

calculate_accuracy_by_problem <- function(theproblem){
  
  files <- list.files("data/feature-calcs", full.names = TRUE, pattern = "\\.Rda")
  message(paste0("Doing problem ", match(theproblem, files), "/", length(files)))
  
  load(theproblem)
  
  # Fit multi-feature classifiers by feature set
  
  results <- fit_multi_feature_classifier(outs, 
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

# List all .Rda files containing features

data_files <- list.files("data/feature-calcs", full.names = TRUE, pattern = "\\.Rda")

# Run function

calculate_accuracy_by_problem_safe <- purrr::possibly(calculate_accuracy_by_problem, otherwise = NULL)

outputs <- data_files %>%
  purrr::map(~ calculate_accuracy_by_problem_safe(theproblem = .x))

# Name list entries for easier viewing and save

classes <- gsub("data/feature-calcs/", "\\1", data_files)
classes <- gsub(".Rda", "\\1", classes)
names(outputs) <- classes
save(outputs, file = "data/outputs.Rda")
