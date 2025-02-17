#------------------------------------------
# This script sets out to compute features
# for each set and time-series problem
#
# NOTE: This script requires setup.R and
# analysis/prepare-time-series-data.R 
# to have been run first
#------------------------------------------

#---------------------------------------
# Author: Trent Henderson, 13 April 2023
#---------------------------------------

# Load data

load("data/TimeSeriesData.Rda")

#------------- Feature extraction --------------

#' Function to map over datasets to avoid massive dataframe processing times / crashes
#' @param theproblem string specifying the problem to calculate features for
#' @returns an object of class feature_calculations
#' @author Trent Henderson
#' 

calculate_features_by_problem <- function(theproblem){
  
  message(paste0("Doing problem ", match(theproblem, keepers), "/", length(keepers)))
  
  # Filter to problem of interest
  
  tmp <- TimeSeriesData %>%
    filter(problem == theproblem) %>%
    dplyr::select(-c(problem, set_split))
  
  tmp <- as_tsibble(tmp, key = c("id", "target"), index = "timepoint")
  
  # Find only problems where each time series is the same length
  
  lengths <- unique(max(tmp$timepoint))
  
  if(length(lengths) == 1){
    
    outs <- try(
      calculate_features(tmp, feature_set = c("catch22", "feasts", 
                                              "tsfeatures", "kats", 
                                              "tsfresh", "tsfel"), 
                         seed = 123)
      )
    
    if("try-error" %in% class(outs)){
    } else{
      
      if(length(outs$feature_set) == 6){
        
        save(outs, file = paste0("data/feature-calcs/", theproblem, ".Rda"))
        
        # z-scored version
        
        tmp_z <- TimeSeriesData %>%
          filter(problem == theproblem) %>%
          dplyr::select(-c(problem, set_split)) %>%
          group_by(id) %>%
          mutate(values = (values - mean(values, na.rm = TRUE)) / sd(values, na.rm = TRUE)) %>%
          ungroup()
        
        tmp_z <- as_tsibble(tmp_z, key = c("id", "target"), index = "timepoint")
        
        outs_z <- calculate_features(tmp_z, feature_set = c("catch22", "feasts", 
                                                            "tsfeatures", "kats", 
                                                            "tsfresh", "tsfel"), 
                                     seed = 123)
        
        save(outs_z, file = paste0("data/feature-calcs/z-scored/", theproblem, ".Rda"))
      }
    }
  }
}

# Run the function for all problems

keepers %>%
  purrr::map(~ calculate_features_by_problem(theproblem = .x))
