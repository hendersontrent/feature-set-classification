#------------------------------------------
# This script sets out to compute features
# for each set and time-series problem
#
# NOTE: This script requires setup.R and
# analysis/prepare-time-series-data.R 
# to have been run first
#-----------------------------------------

#------------------------------------
# Author: Trent Henderson, 5 May 2022
#------------------------------------

# Load data

load("data/TimeSeriesData.Rda")

# Fix Python environment to where the Python libraries are installed on my machine

init_theft("~/opt/anaconda3/bin/python")

#------------- Feature extraction --------------

#' Function to map over datasets to avoid massive dataframe processing times / crashes
#' @param data the dataset containing all raw time series
#' @param theproblem string specifying the problem to calculate features for
#' @param z_score Boolean whether to z-score each time series prior to calculating features. Defaults to \code{FALSE}
#' @returns an object of class dataframe
#' @author Trent Henderson
#' 

extract_features_by_problem <- function(data, theproblem, z_score = TRUE){
  
  message(paste0("Doing problem ", match(theproblem, unique(data$problem)), "/", length(unique(data$problem))))
  
  # Filter to problem of interest
  
  tmp <- data %>%
    filter(problem == theproblem)
  
  if(z_score){
    tmp2 <- tmp %>%
      group_by(id) %>%
      mutate(values = (values - mean(values, na.rm = TRUE)) / sd(values, na.rm = TRUE)) %>%
      ungroup()
    
    if(!all.equal(tmp2$values[1], tmp$values[1])){
      print(paste0("z-score applied to problem: ", theproblem, ". e.g., ", tmp2$values[1], " != ", tmp$values[1]))
      z_tracker <<- append(z_tracker, theproblem) # Keep track of which problems were not z-scored
    }
    
    # Calculate features
    
    outs <- calculate_features(tmp2, id_var = "id", time_var = "timepoint", 
                               values_var = "values", group_var = "target", 
                               feature_set = c("catch22", "feasts", "tsfeatures", "tsfresh", "TSFEL", "Kats"), 
                               catch24 = TRUE, tsfresh_cleanup = FALSE, seed = 123)
  } else{
    
    outs <- calculate_features(tmp, id_var = "id", time_var = "timepoint", 
                               values_var = "values", group_var = "target", 
                               feature_set = c("catch22", "feasts", "tsfeatures", "tsfresh", "TSFEL", "Kats"), 
                               catch24 = TRUE, tsfresh_cleanup = FALSE, seed = 123)
  }
  
  save(outs, file = paste0("data/feature-calcs/", theproblem, ".Rda"))
}

# Run the function

extract_features_by_problem_safe <- purrr::possibly(extract_features_by_problem, otherwise = NULL)
z_tracker <- c()

unique(TimeSeriesData$problem) %>%
  purrr::map(~ extract_features_by_problem_safe(data = TimeSeriesData, theproblem = .x, z_score = TRUE))
