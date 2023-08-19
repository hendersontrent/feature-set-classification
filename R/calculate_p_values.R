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
#' @param problem_data the dataframe contain problem summary information
#' @returns object of class dataframe
#' @author Trent Henderson
#' 

calculate_p_values <- function(data, summary_data, theproblem, all_features = FALSE, problem_data){
  
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
  
  # Check for only 1 feature set present
  
  if(length(unique(tmp_data$method)) <= 1){
    outs <- data.frame(problem = theproblem, statistic = NA, p.value = NA)
    return(outs)
  }
  
  # Check for 0 variance
  
  sd_check <- tmp_data %>%
    group_by(method) %>%
    summarise(stddev = sd(accuracy, na.rm = TRUE)) %>%
    ungroup()
  
  # Set up vectors
  
  x <- tmp_data %>%
    filter(method == tmp_summ_data$best_method) %>%
    pull(accuracy)
  
  y <- tmp_data %>%
    filter(method == tmp_summ_data$worst_method) %>%
    pull(accuracy)
  
  # Filter to get parameters for correlated t-test
  
  params <- problem_data %>%
    filter(problem == unique(tmp_data$problem))
  
  # Do calculation
  
  if(0 %in% sd_check$stddev){
    outs <- data.frame(problem = theproblem, statistic = NA, p.value = NA)
    return(outs)
  } else{
    t_test <- resampled_ttest(x = x, y = y, n = 30, n1 = as.integer(params$Train), n2 = as.integer(params$Test))
    outs <- data.frame(problem = theproblem)
    outs <- cbind(outs, t_test)
    return(outs)
  }
}
