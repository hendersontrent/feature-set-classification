#--------------------------------------
# This script sets out to pull existing
# classifier results for all the 
# problems for comparison
#--------------------------------------

#-------------------------------------
# Author: Trent Henderson, 19 May 2022
#-------------------------------------

#' Function to pull results and wrangle into tidy format
#' @param balanced_accuracy Boolean specifying whether to pull balanced classification accuracies or not
#' @return object of class dataframe
#' @author Trent Henderson
#' 

pull_benchmark_results <- function(balanced_accuracy = TRUE){
  
  # Download results file
  
  url <- "https://www.timeseriesclassification.com/results/AllAccuracies.zip"
  temp <- tempfile()
  download.file(url, temp, mode = "wb")
  
  if(balanced_accuracy){
    path <- "MegaComparison/BALACC/TEST/TESTBALACC_MEANS.csv"
    vals <- "balanced_accuracy"
  } else{
    path <- "MegaComparison/ACC/TEST/TESTACC_MEANS.csv"
    vals <- "accuracy"
  }
  
  # Pull file and tidy up
  
  tmp <- readr::read_csv(unz(temp, filename = path)) %>%
    rename(problem = 1) %>%
    pivot_longer(cols = !problem, names_to = "method", values_to = vals) %>%
    mutate(method = ifelse(method == "Catch22", "catch22", method))
  
  return(tmp)
}
