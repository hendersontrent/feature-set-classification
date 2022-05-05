#------------------------------------------
# This script sets out to compute features
# for each set and time-series problem
#
# NOTE: This script requires setup.R and
# R/tidy_arff_files.R to have been run 
# first
#-----------------------------------------

#------------------------------------
# Author: Trent Henderson, 5 May 2022
#------------------------------------

# Load time series data

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

extract_features_by_problem <- function(data, theproblem){
  
  message("Doing problem ", match(theproblem, unique(data$problem)), "/", length(unique(data$problem)))
  
  # Filter to problem of interest
  
  tmp <- data %>%
    filter(problem == theproblem)
  
  # Calculate features
  
  outs <- calculate_features(tmp, id_var = "id", time_var = "timepoint", 
                             values_var = "values", group_var = "target", 
                             feature_set = c("catch22", "feasts", "tsfeatures", "tsfresh", "tsfel", "kats"), 
                             catch24 = TRUE, tsfresh_cleanup = FALSE, seed = 123)
  
  save(outs, file = paste0("data/feature-calcs/", theproblem, ".Rda"))
}

# Run the function

extract_features_by_problem_safe <- purrr::possibly(extract_features_by_problem, otherwise = NULL)

unique(TimeSeriesData$problem) %>%
  purrr::map(~ extract_features_by_problem_safe(data = TimeSeriesData, theproblem = .x))

#------------- Bind all and store --------------

# Load all .Rda files containing features and row bind together

data_files <- list.files("data/feature-calcs", full.names = TRUE, pattern = "\\.Rda")

#' Function to load a dataset
#' @param x filepath to the .Rda file
#' @returns an object of class dataframe
#' @author Trent Henderson
#' 

load_extracted_features <- function(x){
  load(x)
}

FeatureMatrix <- data_files %>%
  purrr::map_df(~ load_extracted_features(x = .x))

# Save as a single file

save(FeatureMatrix, file = "data/FeatureMatrix.Rda")
