#------------------------------------------
# This script sets out to produce analysis
# of classification performance for sets
# versus all features at once
#
# NOTE: This script requires setup.R and
# analysis/compute-features.R and
# analysis/fit-classifiers.R to have been 
# run first
#-----------------------------------------

#-------------------------------------
# Author: Trent Henderson, 14 May 2022
#-------------------------------------

# Load classification results

load("data/outputs.Rda")
load("data/outputs_aggregated.Rda")

# Remove NULL entries that errored

outputs_filtered <- outputs[!sapply(outputs, is.null)]
outputs_aggregated_filtered <- outputs_aggregated[!sapply(outputs_aggregated, is.null)]
rm(outputs, outputs_aggregated)

#' Pull only main results for every problem and feature set
#' @param results the list containing classification results
#' @param x the index of the problem to get results for
#' @returns an object of class dataframe
#' @author Trent Henderson
#' 

pull_main_models <- function(results, x){
  
  # Filter list
  
  tmp <- results[[x]]
  
  # Extract relevant dataframe and filter to main models
  
  tmp <- tmp$RawClassificationResults %>%
    filter(category == "Main") %>%
    mutate(problem = names(results)[[x]])
  
  return(tmp)
}

# Run the function

main_models <- 1:length(outputs_filtered) %>%
  purrr::map_df(~ pull_main_models(results = outputs_filtered, x = .x)) %>%
  mutate(accuracy = accuracy * 100,
         balanced_accuracy = balanced_accuracy * 100,
         accuracy_sd = accuracy_sd * 100,
         balanced_accuracy_sd = balanced_accuracy_sd * 100) # Just to make the plots nicer

main_models_aggregated <- 1:length(outputs_aggregated_filtered) %>%
  purrr::map_df(~ pull_main_models(results = outputs_aggregated_filtered, x = .x)) %>%
  mutate(accuracy = accuracy * 100,
         balanced_accuracy = balanced_accuracy * 100,
         accuracy_sd = accuracy_sd * 100,
         balanced_accuracy_sd = balanced_accuracy_sd * 100) # Just to make the plots nicer
