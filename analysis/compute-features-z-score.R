#------------------------------------------
# This script sets out to compute features
# for each set and time-series problem
#
# NOTE: This script requires setup.R and
# analysis/prepare-time-series-data.R and
# analysis/mean-and-sd-check.R to have been 
# run first
#-----------------------------------------

#--------------------------------------
# Author: Trent Henderson, 18 June 2022
#--------------------------------------

# Load data

load("data/TimeSeriesData.Rda")

# Fix Python environment to where the Python libraries are installed on my machine

init_theft("~/opt/anaconda3/bin/python")

#------------- Feature extraction --------------

#' Function to map over datasets to avoid massive dataframe processing times / crashes
#' @param data the dataset containing all raw time series
#' @param theproblem string specifying the problem to calculate features for
#' @returns an object of class dataframe
#' @author Trent Henderson
#' 

extract_features_by_problem_z <- function(data, theproblem){
  
  message(paste0("Doing problem ", match(theproblem, unique(data$problem)), "/", length(unique(data$problem))))
  
  # Filter to problem of interest
  
  tmp <- data %>%
    filter(problem == theproblem) %>%
    group_by(id) %>%
    mutate(values = (values - mean(values, na.rm = TRUE)) / sd(values, na.rm = TRUE)) %>%
    ungroup()
    
    # Calculate features
    
    outs_z <- calculate_features(tmp, id_var = "id", time_var = "timepoint", 
                                 values_var = "values", group_var = "target", 
                                 feature_set = c("catch22", "feasts", "tsfeatures", "tsfresh", "TSFEL", "Kats"), 
                                 tsfresh_cleanup = FALSE, seed = 123)
  
  save(outs_z, file = paste0("data/feature-calcs/z-scored/", theproblem, ".Rda"))
}

# Run the function

extract_features_by_problem_z_safe <- purrr::possibly(extract_features_by_problem_z, otherwise = NULL)

unique(TimeSeriesData$problem) %>%
  purrr::map(~ extract_features_by_problem_z_safe(data = TimeSeriesData, theproblem = .x))
