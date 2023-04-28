#' Fit classifiers over resamples and extract performance metrics
#' 
#' @param data \code{data.frame} containing time-series features
#' @param problem_name \code{string} denoting the problem to analyse
#' @param n_resamples \code{integer} denoting the number of resamples to calculate. Defaults to \code{30}
#' @param feature_set \code{character} denoting the feature set to calculate specifically for. Defaults to \code{NULL} for no filtering
#' @return \code{data.frame} of classification performance
#' @author Trent Henderson
#' 

evaluate_performance <- function(data, problem_name, n_resamples = 30, feature_set = NULL){
  
  message(paste0("Doing: ", problem_name, "\n"))
  
  tmp <- data %>%
    filter(problem == problem_name)
  
  if(!is.null(feature_set)){
    tmp <- tmp %>%
      filter(method == feature_set)
  }
  
  tmp <- tmp %>%
    mutate(group = as.factor(as.character(group))) %>%
    dplyr::select(c(id, group, set_split, names, values)) %>%
    pivot_wider(id_cols = c(id, group, set_split), names_from = "names", values_from = "values") %>%
    dplyr::select_if(~sum(!is.na(.)) > 0) %>% # Delete features that are all NaNs
    dplyr::select(where(~dplyr::n_distinct(.) > 1)) %>% # Delete features with constant values
    pivot_longer(cols = -c(id, group, set_split), names_to = "names", values_to = "values")
  
  #------------------ Find good features to retain across resamples ---------------
  
  # Get number of cases in each set
  
  train_rows <- nrow(unique(tmp[tmp$set_split == "Train", 1]))
  test_rows <- nrow(unique(tmp[tmp$set_split == "Test", 1]))
  
  # Get proportion per class in train and test to use for resample procedure
  
  train_props <- tmp %>%
    dplyr::filter(set_split == "Train") %>%
    dplyr::select(c(id, group)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(counter = dplyr::n()) %>%
    dplyr::ungroup()
  
  test_props <- tmp %>%
    dplyr::filter(set_split == "Test") %>%
    dplyr::select(c(id, group)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(counter = dplyr::n()) %>%
    dplyr::ungroup()
  
  #-------------------------------------------------
  # Keep all features that have enough unique values
  # to not ruin models with resampling
  #-------------------------------------------------
  
  # Generate resamples
  
  res_data <- 1:n_resamples %>%
    purrr::map(~ resample_data(tmp, train_rows = train_rows, test_rows = test_rows, train_props, test_props, .x))
  
  # Find only features across all resamples that have SD > 0
  
  good_features <- 1:n_resamples %>%
    purrr::map(~ find_good_features(res_data, .x)) %>%
    unlist()
  
  good_features <- data.frame(names = good_features) %>%
    group_by(names) %>%
    summarise(counter = n()) %>%
    ungroup() %>%
    filter(counter == max(counter)) %>%
    pull(names)
  
  # Filter each resample by the new "good" feature vector
  
  res_data <- 1:n_resamples %>%
    purrr::map(~ filter_good_features(res_data, .x, good_features = good_features))
  
  #---------------- Model fitting ----------------
  
  outs <- 1:n_resamples %>%
    purrr::map_dfr(~ fit_models(res_data, .x)) %>%
    mutate(problem = problem_name)
  
  if(!is.null(feature_set)){
    outs <- outs %>%
      mutate(method = feature_set)
  }
  
  return(outs)
}
