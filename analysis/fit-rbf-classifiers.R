#-----------------------------------------
# This script sets out to compute 
# classification accuracy for RBF SVM
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 13 October 2023
#-----------------------------------------

load("data/good_keepers.Rda")

# Get train-test split labels

load("data/TimeSeriesData.Rda")

train_test_ids <- TimeSeriesData %>%
  dplyr::select(c(problem, id, set_split)) %>%
  distinct()

rm(TimeSeriesData) # Clean up environment as dataframe is large

#------------------ Modelling -----------------

#' Fit RBF SVM classifiers for all feature sets
#' 
#' @param problem_name \code{string} denoting the problem to analyse
#' @param tt_labels \code{data.frame} containing train-test split information
#' @param n_resamples \code{integer} denoting the number of resamples to calculate. Defaults to \code{30}
#' @return \code{data.frame} of classification results
#' @author Trent Henderson
#' 

fit_rbf_models <- function(problem_name, tt_labels, n_resamples = 30){
  
  load(paste0("data/feature-calcs/z-scored/", problem_name, ".Rda"))
  
  tt_labels <- tt_labels %>%
    filter(problem == problem_name)
  
  outs <- outs %>%
    left_join(tt_labels, by = c("id" = "id")) %>%
    filter(names %ni% c("DN_Mean", "DN_Spread_Std")) # Remove mean and variance from catch22
  
  outputs <- unique(outs$method) %>%
    purrr::map_dfr(~ evaluate_performance_rbf(outs, problem_name = problem_name, n_resamples = n_resamples, feature_set = .x))
  
  save(outputs, file = paste0("data/z-score-classifiers/rbfsvm/", problem_name, ".Rda"))
}

# Run function for all problems

good_keepers %>%
  purrr::map_dfr(~ fit_rbf_models(problem_name = .x, tt_labels = train_test_ids, n_resamples = 100))

#---------------- Bind together -----------------

data_files <- list.files("data/z-score-classifiers/rbfsvm", full.names = TRUE, pattern = "\\.Rda")
outputs_z_rbf <- vector(mode = "list", length = length(data_files))

for(d in data_files){
  load(d)
  outputs_z_rbf[[match(d, data_files)]] <- outputs
}

outputs_z_rbf <- do.call("rbind", outputs_z_rbf)
save(outputs_z_rbf, file = "data/outputs_z_rbf.Rda")
rm(outputs)
