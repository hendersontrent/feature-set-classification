#' Fit classifiers for all feature sets
#' 
#' @param problem_name \code{string} denoting the problem to analyse
#' @param tt_labels \code{data.frame} containing train-test split information
#' @param n_resamples \code{integer} denoting the number of resamples to calculate. Defaults to \code{30}
#' @param by_set \code{Boolean} whether to calculate results for each individual feature set. Defaults to \code{TRUE}
#' @param z_scored \code{Boolean} whether the feature data is z-scored or not. Defaults to \code{FALSE}
#' @return \code{data.frame} of classification results
#' @author Trent Henderson
#' 

fit_all_classifiers <- function(problem_name, tt_labels, n_resamples = 30, by_set = TRUE, z_scored = FALSE){
  
  if(z_scored){
    load(paste0("data/feature-calcs/z-scored/", problem_name, ".Rda"))
  } else{
    load(paste0("data/feature-calcs/", problem_name, ".Rda"))
  }
  
  tt_labels <- tt_labels %>%
    filter(problem == problem_name)
  
  outs <- outs %>%
    left_join(tt_labels, by = c("id" = "id")) %>%
    filter(names %ni% c("DN_Mean", "DN_Spread_Std")) # Remove mean and variance from catch22
  
  if(by_set){
    outputs <- unique(outs$method) %>%
      purrr::map_dfr(~ evaluate_performance(outs, problem_name = problem_name, n_resamples = n_resamples, feature_set = .x))
  } else{
    outputs <- evaluate_performance(outs, problem_name = problem_name, n_resamples = n_resamples, feature_set = NULL)
  }
  return(outputs)
}
