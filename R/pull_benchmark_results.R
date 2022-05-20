#--------------------------------------
# This script sets out to pull existing
# classifier results for all the 
# problems for comparison
#--------------------------------------

#-------------------------------------
# Author: Trent Henderson, 19 May 2022
#-------------------------------------

#' Function to pull results and wrangle into tidy format
#' @return object of class dataframe
#' @author Trent Henderson
#' 

pull_benchmark_results <- function(){
  
  # Download results file
  
  url <- "https://www.timeseriesclassification.com/results/AllAccuracies.zip"
  temp <- tempfile()
  download.file(url, temp, mode = "wb")
  
  # Parse results file
  
  
  
  # Clean up into tidy format
}
