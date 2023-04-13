#' Fit classifiers for all feature sets
#' 
#' @param feature_data \code{data.frame} of feature calculations and train-test labels
#' @param problem_name \code{string} denoting the problem to analyse
#' @param n_resamples \code{integer} denoting the number of resamples to calculate. Defaults to \code{30}
#' @param by_set \code{Boolean} whether to calculate results for each individual feature set. Defaults to \code{TRUE}
#' @return \code{data.frame} of classification results
#' @author Trent Henderson
#' 

fit_all_classifiers <- function(feature_data, problem_name, n_resamples = 30, by_set = TRUE){
  if(by_set){
    outputs <- unique(feature_data$method) %>%
      purrr::map_dfr(~ evaluate_performance(feature_data, problem_name = problem_name, n_resamples = n_resamples, feature_set = .x))
  } else{
    outputs <- evaluate_performance(feature_data, problem_name = problem_name, n_resamples = n_resamples, feature_set = NULL)
  }
  return(outputs)
}
