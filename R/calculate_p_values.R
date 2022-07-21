#--------------------------------------
# This script sets defines a function
# to calculate p-values between accuracy
# resamples for two feature sets
#--------------------------------------

#--------------------------------------
# Author: Trent Henderson, 21 July 2022
#--------------------------------------

#' Function to calculate p-values between two feature sets using resamples
#' @param data the dataframe of raw classification accuracy results
#' @param summary_data the dataframe of best and worst results
#' @param theproblem the string name of the problem to calculate for
#' @returns object of class dataframe
#' @author Trent Henderson
#' 

calculate_p_values <- function(data, summary_data, theproblem){
  
  tmp_summ_data <- summary_data %>%
    filter(problem == theproblem)
  
  tmp_data <- data %>%
    filter(problem == theproblem) %>%
    filter(method %in% append(unique(tmp_summ_data$best_method), unique(tmp_summ_data$worst_method)))
  
  # Do calculation
  
  t_test <- t.test(balanced_accuracy ~ method, data = tmp_data, var.equal = FALSE)
  outs <- data.frame(problem = theproblem, t_statistic = t_test$statistic, p_value = t_test$p.value)
}
