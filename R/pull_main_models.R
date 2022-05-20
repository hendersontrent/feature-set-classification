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
#' @param raw Boolean specifying whether to extract raw classification results or test statistics
#' @returns an object of class dataframe
#' @author Trent Henderson
#' 

pull_main_models <- function(results, x, raw = FALSE){
  
  # Filter list
  
  tmp <- results[[x]]
  
  # Extract relevant dataframe and filter to main models
  
  if(raw){
    tmp <- tmp$RawClassificationResults %>%
      filter(category == "Main")
  } else{
    tmp <- tmp$TestStatistics
  }
  
  tmp <- tmp %>%
    mutate(problem = names(results)[[x]])
  
  return(tmp)
}
