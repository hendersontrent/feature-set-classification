#-------------------------------------
# This script sets defines a function
# to extract main model results
#-------------------------------------

#-------------------------------------
# Author: Trent Henderson, 20 May 2022
#-------------------------------------

#' Pull only main results for every problem and feature set
#' @param results the list containing classification results
#' @param x the index of the problem to get results for
#' @returns an object of class dataframe
#' @author Trent Henderson
#' 

pull_main_models <- function(results, x){
  
  # Filter list
  
  tmp <- results[[x]]
  
  # Extract relevant dataframe
  
  tmp <- tmp$RawClassificationResults %>%
    mutate(problem = names(results)[[x]]) %>%
    filter(category == "Main")
  
  return(tmp)
}
