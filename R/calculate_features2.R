#------------------- Helper functions to reduce length -------------

#--------
# catch22
#--------

calc_catch22_2 <- function(data, catch24){
  
  if("group" %in% colnames(data)){
    outData <- data %>%
      tibble::as_tibble() %>%
      dplyr::group_by(.data$id, .data$group) %>%
      dplyr::arrange(.data$timepoint) %>%
      dplyr::summarise(Rcatch22::catch22_all(.data$values, catch24 = catch24)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(method = "catch22")
  } else{
    outData <- data %>%
      tibble::as_tibble() %>%
      dplyr::group_by(.data$id) %>%
      dplyr::arrange(.data$timepoint) %>%
      dplyr::summarise(Rcatch22::catch22_all(.data$values, catch24 = catch24)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(method = "catch22")
  }
  
  message("\nCalculations completed for catch22.")
  return(outData)
}

#-------
# feasts
#-------

calc_feasts_2 <- function(data){
  
  if("group" %in% colnames(data)){
    tsData <- tsibble::as_tsibble(data, key = c(.data$id, .data$group), index = .data$timepoint)
    
    outData <- tsData %>%
      fabletools::features(.data$values, fabletools::feature_set(pkgs = "feasts"))  %>%
      tidyr::gather("names", "values", -c(.data$id, .data$group)) %>%
      dplyr::mutate(method = "feasts")
  } else{
    tsData <- tsibble::as_tsibble(data, key = c(.data$id), index = .data$timepoint)
    
    outData <- tsData %>%
      fabletools::features(.data$values, fabletools::feature_set(pkgs = "feasts"))  %>%
      tidyr::gather("names", "values", -.data$id) %>%
      dplyr::mutate(method = "feasts")
  }
  
  message("\nCalculations completed for feasts.")
  return(outData)
}

#-----------
# tsfeatures
#-----------

tsfeatures_helper_2 <- function(data, grouped = FALSE, feats){
  
  if(grouped){
    vars <- c("id", "group")
  } else{
    vars <- c("id")
  }
  
  outData <- data %>%
    tibble::as_tibble() %>%
    dplyr::group_by_at(dplyr::all_of(vars)) %>%
    dplyr::arrange(.data$timepoint) %>%
    dplyr::select(-c(.data$timepoint)) %>%
    dplyr::summarise(values = list(.data$values)) %>%
    dplyr::group_by_at(dplyr::all_of(vars)) %>%
    dplyr::summarise(tsfeatures::tsfeatures(.data$values, features = feats)) %>%
    dplyr::ungroup() %>%
    tidyr::gather("names", "values", -c(dplyr::all_of(vars))) %>%
    dplyr::mutate(method = "tsfeatures")
  
  return(outData)
}

calc_tsfeatures_2 <- function(data){
  
  featureList <- c("frequency", "stl_features", "entropy", "acf_features",
                   "compengine", "arch_stat", "crossing_points", "flat_spots",
                   "heterogeneity", "holt_parameters", "hurst", 
                   "lumpiness", "max_kl_shift", "max_level_shift", "max_var_shift", 
                   "nonlinearity", "pacf_features", "stability", "unitroot_kpss",
                   "unitroot_pp", "embed2_incircle", "firstzero_ac",
                   "histogram_mode", "localsimple_taures", "sampenc",
                   "spreadrandomlocal_meantaul")
  
  if("group" %in% colnames(data)){
    outData <- try(tsfeatures_helper_2(data = data, grouped = TRUE, feats = featureList))
    
    if("try-error" %in% class(outData)){
      
      message("Removing 'compengine' features from tsfeatures due to length error. Recomputing with reduced set...")
      featureList <- featureList[!featureList %in% c("compengine")]
      outData <- try(tsfeatures_helper_2(data = data, grouped = TRUE, feats = featureList))
    }
    
  } else{
    outData <- try(tsfeatures_helper_2(data = data, grouped = FALSE, feats = featureList))
    
    if("try-error" %in% class(outData)){
      
      message("Removing 'compengine' features from tsfeatures due to length error. Recomputing with reduced set...")
      featureList <- featureList[!featureList %in% c("compengine")]
      outData <- try(tsfeatures_helper_2(data = data, grouped = FALSE, feats = featureList))
    }
  }
  
  message("\nCalculations completed for tsfeatures.")
  return(outData)
}

#--------
# tsfresh
#--------

calc_tsfresh_2 <- function(data, column_id = "id", column_sort = "timepoint", cleanup){
  
  if("group" %in% colnames(data)){
    groups <- data %>%
      dplyr::select(c(.data$id, .data$group)) %>%
      dplyr::distinct()
  }
  
  # Load Python function
  
  tsfresh_calculator <- function(){}
  reticulate::source_python(system.file("python", "tsfresh_calculator.py", package = "theft")) # Ships with package
  
  # Convert time index column to numeric to avoid {tsfresh} errors
  
  if(!is.numeric(data$id) || !is.numeric(data$timepoint)){
    
    ids <- data.frame(old_id = unique(data$id)) %>%
      dplyr::mutate(id = dplyr::row_number())
    
    temp <- data %>%
      dplyr::rename(old_id = id) %>%
      dplyr::left_join(ids, by = c("old_id" = "old_id")) %>%
      dplyr::group_by(.data$id) %>%
      dplyr::arrange(.data$timepoint) %>%
      dplyr::mutate(timepoint = as.numeric(dplyr::row_number())) %>%
      dplyr::ungroup()
    
    # Dropping columns with dplyr::select() isn't working, so just make a new dataframe
    
    temp1 <- data.frame(id = temp$id,
                        timepoint = temp$timepoint,
                        values = temp$values)
    
    if("group" %in% colnames(data)){
      
      classes <- groups %>%
        dplyr::select(c(.data$group)) %>%
        dplyr::mutate(id = dplyr::row_number())
      
      outData <- tsfresh_calculator(timeseries = temp1, column_id = column_id, column_sort = column_sort, cleanup = cleanup, classes = classes)
    } else{
      outData <- tsfresh_calculator(timeseries = temp1, column_id = column_id, column_sort = column_sort, cleanup = cleanup)
    }
    
    # Compute features and re-join back correct id labels
    
    ids2 <- ids %>%
      dplyr::select(-c(.data$id)) %>%
      dplyr::rename(id = .data$old_id)
    
    outData <- outData %>%
      cbind(ids2) %>%
      tidyr::gather("names", "values", -.data$id) %>%
      dplyr::mutate(method = "tsfresh")
    
  } else{
    temp1 <- data.frame(id = data$id,
                        timepoint = data$timepoint,
                        values = data$values)
    
    ids <- unique(temp1$id)
    
    if("group" %in% colnames(data)){
      
      classes <- groups %>%
        dplyr::select(c(.data$group)) %>%
        dplyr::mutate(id = dplyr::row_number())
      
      outData <- tsfresh_calculator(timeseries = temp1, column_id = column_id, column_sort = column_sort, cleanup = cleanup, clases = classes) 
    } else{
      outData <- tsfresh_calculator(timeseries = temp1, column_id = column_id, column_sort = column_sort, cleanup = cleanup) 
    }
    
    # Do calculations
    
    outData <- outData %>%
      dplyr::mutate(id = ids) %>%
      tidyr::gather("names", "values", -.data$id) %>%
      dplyr::mutate(method = "tsfresh")
  }
  
  if(c("group") %in% colnames(data)){
    outData <- outData %>%
      dplyr::inner_join(groups, by = c("id" = "id"))
  }
  
  message("\nCalculations completed for tsfresh.")
  return(outData)
}

#------
# TSFEL
#------

calc_tsfel_2 <- function(data){
  
  # Load Python function
  
  tsfel_calculator <- function(){}
  reticulate::source_python(system.file("python", "tsfel_calculator.py", package = "theft")) # Ships with package
  
  if("group" %in% colnames(data)){
    outData <- data %>%
      tibble::as_tibble() %>%
      dplyr::group_by(.data$id, .data$group) %>%
      dplyr::arrange(.data$timepoint) %>%
      dplyr::summarise(tsfel_calculator(.data$values)) %>%
      dplyr::ungroup() %>%
      tidyr::gather("names", "values", -c(.data$id, .data$group)) %>%
      dplyr::mutate(method = "TSFEL")
  } else{
    outData <- data %>%
      tibble::as_tibble() %>%
      dplyr::group_by(.data$id) %>%
      dplyr::arrange(.data$timepoint) %>%
      dplyr::summarise(tsfel_calculator(.data$values)) %>%
      dplyr::ungroup() %>%
      tidyr::gather("names", "values", -c(.data$id)) %>%
      dplyr::mutate(method = "TSFEL")
  }
  
  message("\nCalculations completed for TSFEL.")
  return(outData)
}

#-----
# Kats
#-----

calc_kats_2 <- function(data){
  
  # Load Python function
  
  kats_calculator <- function(){}
  reticulate::source_python(system.file("python", "kats_calculator.py", package = "theft")) # Ships with package
  
  # Convert numeric time index to datetime as Kats requires it
  
  unique_times <- unique(data$timepoint)
  
  datetimes <- data.frame(timepoint = unique_times) %>%
    dplyr::mutate(time = seq(as.Date("1800-01-01"), by = "day", length.out = length(unique_times)))
  
  # Join in datetimes and run computations
  
  if("group" %in% colnames(data)){
    outData <- data %>%
      dplyr::left_join(datetimes, by = c("timepoint" = "timepoint")) %>%
      dplyr::select(-c(.data$timepoint)) %>%
      dplyr::group_by(.data$id, .data$group) %>%
      dplyr::arrange(.data$time) %>%
      dplyr::summarise(results = list(kats_calculator(timepoints = .data$time, values = .data$values))) %>%
      tidyr::unnest_wider(.data$results) %>%
      dplyr::ungroup() %>%
      tidyr::gather("names", "values", -c(.data$id, .data$group)) %>%
      dplyr::mutate(method = "Kats")
  } else{
    outData <- data %>%
      dplyr::left_join(datetimes, by = c("timepoint" = "timepoint")) %>%
      dplyr::select(-c(.data$timepoint)) %>%
      dplyr::group_by(.data$id) %>%
      dplyr::arrange(.data$time) %>%
      dplyr::summarise(results = list(kats_calculator(timepoints = .data$time, values = .data$values))) %>%
      tidyr::unnest_wider(.data$results) %>%
      dplyr::ungroup() %>%
      tidyr::gather("names", "values", -c(.data$id)) %>%
      dplyr::mutate(method = "Kats")
  }
  
  message("\nCalculations completed for Kats.")
  return(outData)
}

#------------------- Main exported calculation function ------------

#' Compute features on an input time series dataset
#' 
#' @importFrom rlang .data
#' @import dplyr
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather pivot_longer unnest_wider
#' @import tsibble
#' @import Rcatch22
#' @importFrom tsfeatures lumpiness stability max_level_shift max_var_shift max_kl_shift crossing_points flat_spots hurst compengine autocorr_features pred_features station_features dist_features scal_features embed2_incircle firstzero_ac ac_9 firstmin_ac trev_num motiftwo_entro3 binarize_mean walker_propcross localsimple_taures sampen_first sampenc std1st_der spreadrandomlocal_meantaul histogram_mode outlierinclude_mdrmd fluctanal_prop_r1 entropy tsfeatures stl_features acf_features pacf_features holt_parameters hw_parameters heterogeneity nonlinearity arch_stat
#' @import feasts
#' @import reticulate
#' @importFrom fabletools features
#' @importFrom fabletools feature_set
#' @param data \code{data.frame} with at least 4 columns: id variable, group variable, time variable, value variable
#' @param id_var \code{string} specifying the ID variable to identify each time series. Defaults to \code{"id"}
#' @param time_var \code{string} specifying the time index variable. Defaults to \code{"timepoint"}
#' @param values_var \code{string} specifying the values variable. Defaults to \code{"values"}
#' @param group_var \code{string} specifying the grouping variable that each unique series sits under (if one exists). Defaults to \code{NULL}
#' @param feature_set \code{string} or \code{vector} of \code{string}s denoting the set of time-series features to calculate. Defaults to \code{"catch22"}
#' @param catch24 \code{Boolean} specifying whether to compute \code{catch24} in addition to \code{catch22} if \code{catch22} is one of the feature sets selected. Defaults to \code{FALSE}
#' @param tsfresh_cleanup \code{Boolean} specifying whether to use the in-built \code{tsfresh} relevant feature filter or not. Defaults to \code{FALSE}
#' @param seed \code{integer} denoting a fixed number for R's random number generator to ensure reproducibility
#' @param the_id \code{string} denoting the ID in the dataset to compute features for. Defaults to \code{NULL}
#' @return object of class \code{feature_calculations} that contains the summary statistics for each feature
#' @author Trent Henderson
#' @export
#' @examples
#' 

calculate_features2 <- function(data, id_var = "id", time_var = "timepoint", values_var = "values", group_var = NULL,
                                feature_set = c("catch22", "feasts", "tsfeatures", "Kats", "tsfresh", "TSFEL"), 
                                catch24 = FALSE, tsfresh_cleanup = FALSE, seed = 123, the_id = NULL){
  
  #--------- Error catches ---------
  
  #-----------------
  # Method selection
  #-----------------
  
  # Recode deprecated lower case from v0.3.5
  
  feature_set <- replace(feature_set, feature_set == "kats", "Kats")
  feature_set <- replace(feature_set, feature_set == "tsfel", "TSFEL")
  
  #--------- Renaming --------
  
  data_re <- data %>%
    dplyr::rename(id = dplyr::all_of(id_var),
                  timepoint = dplyr::all_of(time_var),
                  values = dplyr::all_of(values_var)) %>%
    dplyr::filter(id == the_id)
  
  if(!is.null(group_var)){
    data_re <- data_re %>%
      dplyr::rename(group = dplyr::all_of(group_var)) %>%
      dplyr::select(c(.data$id, .data$timepoint, .data$values, .data$group))
  } else{
    data_re <- data_re %>%
      dplyr::select(c(.data$id, .data$timepoint, .data$values))
  }
  
  data_re <- data_re %>%
    dplyr::arrange(.data$timepoint) %>%
    tidyr::drop_na()
  
  #--------- Feature calcs --------
  
  if("catch22" %in% feature_set){
    
    message("Running computations for catch22...")
    tmp_catch22 <- calc_catch22_2(data = data_re, catch24 = catch24)
  }
  
  if("feasts" %in% feature_set){
    
    message("Running computations for feasts...")
    tmp_feasts <- calc_feasts_2(data = data_re)
  }
  
  if("tsfeatures" %in% feature_set){
    
    message("Running computations for tsfeatures..")
    
    if(max(data_re$timepoint) < 50){
      message("Time series is too short for tsfeatures. Removing from calculations.")
    } else{
      tmp_tsfeatures <- calc_tsfeatures_2(data = data_re)
    }
  }
  
  if("tsfresh" %in% feature_set){
    
    if(tsfresh_cleanup){
      cleanuper <- "Yes"
    } else{
      cleanuper <- "No"
    }
    
    message("\nRunning computations for tsfresh...")
    tmp_tsfresh <- calc_tsfresh_2(data = data_re, column_id = "id", column_sort = "timepoint", cleanup = cleanuper)
  }
  
  if("TSFEL" %in% feature_set){
    
    message("\nRunning computations for TSFEL...")
    tmp_tsfel <- calc_tsfel_2(data = data_re)
  }
  
  if("Kats" %in% feature_set){
    
    message("\nRunning computations for Kats...")
    tmp_kats <- calc_kats_2(data = data_re)
  }
  
  tmp_all_features <- data.frame()
  
  if(length(feature_set) > 1){
    message("\nBinding feature dataframes together...")
  }
  
  if(exists("tmp_catch22")){
    tmp_all_features <- dplyr::bind_rows(tmp_all_features, tmp_catch22)
  }
  
  if(exists("tmp_feasts")){
    tmp_all_features <- dplyr::bind_rows(tmp_all_features, tmp_feasts)
  }
  
  if(exists("tmp_tsfeatures")){
    tmp_all_features <- dplyr::bind_rows(tmp_all_features, tmp_tsfeatures)
  }
  
  if(exists("tmp_tsfresh")){
    tmp_all_features <- dplyr::bind_rows(tmp_all_features, tmp_tsfresh)
  }
  
  if(exists("tmp_tsfel")){
    tmp_all_features <- dplyr::bind_rows(tmp_all_features, tmp_tsfel)
  }
  
  if(exists("tmp_kats")){
    tmp_all_features <- dplyr::bind_rows(tmp_all_features, tmp_kats)
  }
  
  tmp_all_features <- structure(list(tmp_all_features), class = "feature_calculations")
  return(tmp_all_features)
}
