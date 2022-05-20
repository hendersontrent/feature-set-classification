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
  path_bal <- "MegaComparison/BALACC/TEST/TESTBALACC_MEANS.csv"
  vals_bal <- "balanced_accuracy"
  path <- "MegaComparison/ACC/TEST/TESTACC_MEANS.csv"
  vals <- "accuracy"
  
  # Pull file and tidy up
  
  tmp_bal <- readr::read_csv(unz(temp, filename = path_bal)) %>%
    rename(problem = 1) %>%
    pivot_longer(cols = !problem, names_to = "method", values_to = vals_bal) %>%
    mutate(method = ifelse(method == "Catch22", "catch22", method))
  
  tmp_acc <- readr::read_csv(unz(temp, filename = path)) %>%
    rename(problem = 1) %>%
    pivot_longer(cols = !problem, names_to = "method", values_to = vals) %>%
    mutate(method = ifelse(method == "Catch22", "catch22", method))
  
  tmp <- tmp_acc %>%
    left_join(tmp_bal, by = c("problem" = "problem", "method" = "method"))
  
  return(tmp)
}
