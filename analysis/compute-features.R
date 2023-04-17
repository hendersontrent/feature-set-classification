#------------------------------------------
# This script sets out to compute features
# for each set and time-series problem
#
# NOTE: This script requires setup.R and
# analysis/prepare-time-series-data.R 
# to have been run first
#-----------------------------------------

#---------------------------------------
# Author: Trent Henderson, 13 April 2023
#---------------------------------------

# Load data

load("data/TimeSeriesData.Rda")

# Fix Python environment to where the Python libraries are installed on my machine

reticulate::use_virtualenv("/Users/trenthenderson/Documents/Git/feature-set-classification/venv")

#------------- Feature extraction --------------

#' Function to map over datasets to avoid massive dataframe processing times / crashes
#' @param data the dataset containing all raw time series
#' @param theproblem string specifying the problem to calculate features for
#' @returns an object of class dataframe
#' @author Trent Henderson
#' 

extract_features_by_problem <- function(data, theproblem){
  
  message(paste0("Doing problem ", match(theproblem, unique(data$problem)), "/", length(unique(data$problem))))
  
  # Filter to problem of interest
  
  tmp <- data %>%
    filter(problem == theproblem)
  
  # Calculate features
  
  outs <- calculate_features(tmp, id_var = "id", time_var = "timepoint", 
                             values_var = "values", group_var = "target", 
                             feature_set = c("catch22", "feasts", "tsfeatures", "tsfresh", "TSFEL"), 
                             catch24 = TRUE, seed = 123)[[1]]
  
  # Catch cases where appended NAs cause errors (i.e., different time series have different lengths)
  # We do this by mapping over IDs to a modified feature calculation function that drops NAs by ID
  
  if(length(unique(tmp$id)) != length(unique(outs$id))){
    outs <- unique(tmp$id) %>%
      purrr::map_dfr(~ calculate_features2(tmp, id_var = "id", time_var = "timepoint", 
                                           values_var = "values", group_var = "target", 
                                           feature_set = c("catch22", "feasts", "tsfeatures", "tsfresh", "TSFEL"), 
                                           catch24 = TRUE, seed = 123, the_id = .x))[[1]]
  }
  
  save(outs, file = paste0("data/feature-calcs/", theproblem, ".Rda"))
}

# Run the function

unique(TimeSeriesData$problem)[!unique(TimeSeriesData$problem) %in% c("AllGestureWiimoteX", "AllGestureWiimoteY", "AllGestureWiimoteZ", "PLAID")] %>%
  purrr::map(~ extract_features_by_problem(data = TimeSeriesData, theproblem = .x))
