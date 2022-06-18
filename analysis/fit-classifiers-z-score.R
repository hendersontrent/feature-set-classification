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
#' @param remove_catch24 Boolean whether to remove mean and SD from catch22 feature set
#' @returns an object of class dataframe
#' @author Trent Henderson
#' 

calculate_accuracy_by_problem <- function(theproblem, tt_labels, set = TRUE, remove_catch24 = TRUE){
  
  files <- list.files("data/feature-calcs", full.names = TRUE, pattern = "\\.Rda")
  message(paste0("Doing problem ", match(theproblem, files), "/", length(files)))
  
  load(theproblem)
  
  # Remove Mean and SD from catch22 if specified (e.g., for un-normalised data)
  
  if(remove_catch24){
    outs <- outs %>%
      filter(names %ni% c("DN_Mean", "DN_Spread_Std"))
  }
  
  # Join in train-test indicator
  
  outs <- outs %>%
    inner_join(tt_labels, by = c("id" = "id")) %>%
    dplyr::select(-c(problem))
  
  # Fit multi-feature classifiers by feature set
  
  results <- fit_multi_feature_classifier_tt(outs, 
                                             id_var = "id", 
                                             group_var = "group",
                                             by_set = set, 
                                             test_method = "svmLinear", 
                                             use_balanced_accuracy = TRUE,
                                             use_k_fold = TRUE, 
                                             num_folds = 10, 
                                             num_resamples = 30)
  
  return(results)
}

calculate_accuracy_by_problem_safe <- purrr::possibly(calculate_accuracy_by_problem, otherwise = NULL)

data_files <- list.files("data/feature-calcs/z-score", full.names = TRUE, pattern = "\\.Rda")

outputs_z <- data_files %>%
  purrr::map_df(~ calculate_accuracy_by_problem_safe(theproblem = .x, tt_labels = train_test_ids, set = TRUE, remove_catch24 = TRUE))

# Run function using all features at once to form an aggregate comparison later

outputs_aggregate_z <- data_files %>%
  purrr::map_df(~ calculate_accuracy_by_problem_safe(theproblem = .x, tt_labels = train_test_ids, set = FALSE, remove_catch24 = TRUE))

save(outputs_z, file = "data/outputs_z.Rda")
save(outputs_aggregate_z, file = "data/outputs_aggregate_z.Rda") 
