#------------------------------------------
# This script sets defines a function for 
# calculating the correlated t-test statistic
# presented in Nadeau & Bengio (2003)
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 21 November 2022
#------------------------------------------

corr_t_test <- function(x, y, n, n1, n2){
  
  d <- y - x # Calculate differences
  d_bar <- mean(d, na.rm = TRUE) # Calculate mean of differences
  sigma_2 <- var(d, na.rm = TRUE) # Calculate variance
  sigma_2_mod <- sigma_2 * (1/n + n1/n2) # Calculate modified variance
  t_stat <- d_bar / sqrt(sigma_2_mod) # Calculate t-statistic
  
  if(t_stat < 0){
    p_val <- pt(t_stat, n - 1) # p-value for left tail
  } else{
    p_val <- pt(t_stat, n - 1, lower.tail = FALSE) # p-value for right tail
  }
  
  return(p_val)
}
