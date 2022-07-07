#------------------------------------------
# This script sets out to compute 
# classification accuracy for each feature 
# set by problem
#
# NOTE: This script requires setup.R and
# analysis/compute-features.R and
# analysis/compute-features-z-score.R to 
# have been run first
#-----------------------------------------

#--------------------------------------
# Author: Trent Henderson, 18 June 2022
#--------------------------------------

# Load in data and summarise to just problem, ID, and train-test set indicator as I didn't bind initially

load("data/TimeSeriesData.Rda")

train_test_ids <- TimeSeriesData %>%
  dplyr::select(c(problem, id, set_split)) %>%
  distinct()

rm(TimeSeriesData) # Clean up environment as dataframe is large

#---------------- Classification accuracy -----------------

#' Function to map classification performance calculations over datasets/problems
#' @param theproblem filepath to the feature data
#' @param tt_labels the dataframe containing train-test labels
#' @param set Boolean whether to fit by set or not
#' @returns an object of class dataframe
#' @author Trent Henderson
#' 

calculate_accuracy_by_problem_z <- function(theproblem, tt_labels, set = TRUE){
  
  files <- list.files("data/feature-calcs/z-scored", full.names = TRUE, pattern = "\\.Rda")
  message(paste0("Doing problem ", match(theproblem, files), "/", length(files)))
  load(theproblem)
  problem_name <- gsub(".*/", "\\1", theproblem)
  problem_name <- gsub(".Rda", "\\1", problem_name)
  
  # Join in train-test indicator
  
  outs_z <- outs_z %>%
    inner_join(tt_labels, by = c("id" = "id")) %>%
    dplyr::select(-c(problem))
  
  # Fit multi-feature classifiers by feature set
  
  results <- fit_multi_feature_classifier_tt(outs_z, 
                                             id_var = "id", 
                                             group_var = "group",
                                             by_set = set, 
                                             test_method = "svmLinear", 
                                             use_balanced_accuracy = TRUE,
                                             use_k_fold = TRUE, 
                                             num_folds = 10, 
                                             num_resamples = 30,
                                             problem_name = problem_name) %>%
    mutate(problem = problem_name)
  
  return(results)
}

calculate_accuracy_by_problem_z_safe <- purrr::possibly(calculate_accuracy_by_problem_z, otherwise = NULL)
data_files <- list.files("data/feature-calcs/z-scored", full.names = TRUE, pattern = "\\.Rda")

outputs_z <- data_files %>%
  purrr::map_df(~ calculate_accuracy_by_problem_z_safe(theproblem = .x, tt_labels = train_test_ids, set = TRUE))

# Run function using all features at once to form an aggregate comparison later

outputs_aggregate_z <- data_files %>%
  purrr::map_df(~ calculate_accuracy_by_problem_z_safe(theproblem = .x, tt_labels = train_test_ids, set = FALSE))

save(outputs_z, file = "data/outputs_z.Rda")
save(outputs_aggregate_z, file = "data/outputs_aggregate_z.Rda") 
