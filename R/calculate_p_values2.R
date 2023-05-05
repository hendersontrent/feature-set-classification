#--------------------------------------
# This script defines a function to 
# calculate p-values between accuracy
# resamples for two feature sets
#--------------------------------------

#--------------------------------------
# Author: Trent Henderson, 29 July 2022
#--------------------------------------

#' Function to calculate p-values between two feature sets using resamples
#' @param data the dataframe of raw classification accuracy results
#' @param combn_data the dataframe of pairwise combinations
#' @param rownum the row number of combn_data to use
#' @param problem_data the dataframe contain problem summary information
#' @returns object of class dataframe
#' @author Trent Henderson
#' 

calculate_p_values2 <- function(data, combn_data, rownum, problem_data){
  
  combn_filt <- combn_data[rownum, ]
  
  # Filter data
  
  tmp_data <- data %>%
    filter(problem == combn_filt$problem) %>%
    filter(method %in% append(combn_filt$set1, combn_filt$set2))
  
  # Check for only 1 feature set present
  
  if(length(unique(tmp_data$method)) <= 1){
    outs <- data.frame(problem = combn_filt$problem, method = combn_filt$set1, statistic = NA, p.value = NA)
    return(outs)
  }
  
  # Check for lack of observations
  
  group_check <- tmp_data %>%
    group_by(method) %>%
    summarise(resamples = n()) %>%
    ungroup()
  
  if(min(group_check$resamples) <= 2){
    outs <- data.frame(problem = combn_filt$problem, method = combn_filt$set1, statistic = NA, p.value = NA)
    return(outs)
  }
  
  # Check for 0 variance
  
  sd_check <- tmp_data %>%
    group_by(method) %>%
    summarise(stddev = sd(balanced_accuracy, na.rm = TRUE)) %>%
    ungroup()
  
  # Set up vectors
  
  x <- tmp_data %>%
    filter(method == combn_filt$set1) %>%
    pull(balanced_accuracy)
  
  y <- tmp_data %>%
    filter(method == combn_filt$set2) %>%
    pull(balanced_accuracy)
  
  # Filter to get parameters for correlated t-test
  
  params <- problem_data %>%
    filter(problem == unique(tmp_data$problem))
  
  # Do calculation
  
  if(0 %in% sd_check$stddev){
    outs <- data.frame(problem = combn_filt$problem, method = combn_filt$set1, statistic = NA, p.value = NA)
    return(outs)
  } else{
    t_test <- resampled_ttest(x = x, y = y, n = 30, n1 = as.integer(params$Train), n2 = as.integer(params$Test))
    outs <- data.frame(problem = combn_filt$problem, method = combn_filt$set1)
    outs <- cbind(outs, t_test)
    return(outs)
  }
}
