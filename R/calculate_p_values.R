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
#' @param all_features Boolean whether "All features" is part of the analysis
#' @returns object of class dataframe
#' @author Trent Henderson
#' 

calculate_p_values <- function(data, summary_data, theproblem, all_features = FALSE){
  
  tmp_summ_data <- summary_data %>%
    filter(problem == theproblem)
  
  if(all_features){
    themethods <- append(unique(tmp_summ_data$method_set), unique(tmp_summ_data$method))
  } else{
    themethods <- append(unique(tmp_summ_data$best_method), unique(tmp_summ_data$worst_method))
  }
  
  tmp_data <- data %>%
    filter(problem == theproblem) %>%
    filter(method %in% themethods)
  
  # Check for 0 variance
  
  sd_check <- tmp_data %>%
    group_by(method) %>%
    summarise(stddev = sd(balanced_accuracy, na.rm = TRUE)) %>%
    ungroup()
  
  # Do calculation
  
  if(0 %in% sd_check$stddev){
    outs <- data.frame(problem = theproblem, t_statistic = NA, p_value = NA)
    return(outs)
  } else{
    t_test <- t.test(balanced_accuracy ~ method, data = tmp_data, var.equal = FALSE)
    outs <- data.frame(problem = theproblem, t_statistic = t_test$statistic, p_value = t_test$p.value)
    return(outs)
  }
}
