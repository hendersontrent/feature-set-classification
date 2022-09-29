#--------------------------------------
# This script sets defines a function
# to calculate p-values between accuracy
# resamples for two feature sets
#--------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 28 September 2022
#-------------------------------------------

#' Function to calculate p-values between two feature sets using resamples
#' @param data the dataframe of raw classification accuracy results
#' @param combn_data the dataframe of pairwise combinations
#' @param rownum the row number of combn_data to use
#' @returns object of class dataframe
#' @author Trent Henderson
#' 

calculate_p_values_acc <- function(data, combn_data, rownum){
  
  combn_filt <- combn_data[rownum, ]
  
  # Filter data
  
  tmp_data <- data %>%
    filter(problem == combn_filt$problem) %>%
    filter(method %in% append(combn_filt$set1, combn_filt$set2))
  
  # Check for only 1 feature set present
  
  if(length(unique(tmp_data$method)) <= 1){
    outs <- data.frame(problem = combn_filt$problem, set1 = combn_filt$set1, set2 = combn_filt$set2, t_statistic = NA, p_value = NA)
    return(outs)
  }
  
  # Check for lack of observations
  
  group_check <- tmp_data %>%
    group_by(method) %>%
    summarise(resamples = n()) %>%
    ungroup()
  
  if(min(group_check$resamples) <= 2){
    outs <- data.frame(problem = combn_filt$problem, set1 = combn_filt$set1, set2 = combn_filt$set2, t_statistic = NA, p_value = NA)
    return(outs)
  }
  
  # Check for 0 variance
  
  sd_check <- tmp_data %>%
    group_by(method) %>%
    summarise(stddev = sd(accuracy, na.rm = TRUE)) %>%
    ungroup()
  
  # Do calculation
  
  if(0 %in% sd_check$stddev){
    outs <- data.frame(problem = combn_filt$problem, set1 = combn_filt$set1, set2 = combn_filt$set2, t_statistic = NA, p_value = NA)
    return(outs)
  } else{
    t_test <- t.test(accuracy ~ method, data = tmp_data, var.equal = FALSE)
    outs <- data.frame(problem = combn_filt$problem, set1 = combn_filt$set1, set2 = combn_filt$set2, 
                       t_statistic = t_test$statistic, p_value = t_test$p.value)
    return(outs)
  }
}
