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
#' @param data the dataset containing all raw time series
#' @param theproblem string specifying the problem to calculate features for
#' @returns an object of class dataframe
#' @author Trent Henderson
#' 

extract_features_by_problem <- function(data, theproblem){
  
  message(paste0("Doing problem ", match(theproblem, keepers), "/", length(keepers)))
  
  # Filter to problem of interest
  
  tmp <- data %>%
    filter(problem == theproblem)
  
  # Calculate features
  
  outs <- try(calculate_features(tmp, id_var = "id", time_var = "timepoint", 
                                 values_var = "values", group_var = "target", 
                                 feature_set = c("catch22", "feasts", "tsfeatures", "Kats", "tsfresh", "TSFEL"), 
                                 catch24 = TRUE, seed = 123)[[1]])
  
  # Catch cases where appended NAs cause errors (i.e., different time series have different lengths or a tsfeatures errors due to time-series length)
  
  if("try-error" %in% class(outs) || (length(unique(tmp$id)) != length(unique(outs$id)))){
    
    outs <- try(calculate_features(tmp, id_var = "id", time_var = "timepoint", 
                                   values_var = "values", group_var = "target", 
                                   feature_set = c("catch22", "feasts", "Kats", "tsfresh", "TSFEL"), 
                                   catch24 = TRUE, seed = 123)[[1]])
    
    if("try-error" %in% class(outs) || (length(unique(tmp$id)) != length(unique(outs$id)))){
      outs <- unique(tmp$id) %>%
        purrr::map_dfr(~ calculate_features2(tmp, id_var = "id", time_var = "timepoint", 
                                             values_var = "values", group_var = "target", 
                                             feature_set = c("catch22", "feasts", "tsfeatures", "Kats", "tsfresh", "TSFEL"), 
                                             catch24 = TRUE, seed = 123, the_id = .x)[[1]])
    }
  }
  
  save(outs, file = paste0("data/feature-calcs/", theproblem, ".Rda"))
}

# Run the function

keepers %>%
  purrr::map(~ extract_features_by_problem(data = TimeSeriesData, theproblem = .x))

#--------------- Get final list of problems that calculated successfully -----------------

features <- list.files("data/feature-calcs", full.names = TRUE, pattern = "\\.Rda") %>%
  purrr::map_dfr(~ get_feature_data(.x))

# Load in data and summarise to just problem, ID, and train-test set indicator as I didn't bind initially

train_test_ids <- TimeSeriesData %>%
  dplyr::select(c(problem, id, set_split)) %>%
  distinct()

rm(TimeSeriesData) # Clean up environment as dataframe is large

features <- features %>%
  left_join(train_test_ids, by = c("id" = "id", "problem" = "problem"))

save(features, file = "data/feature-calcs/bound/features.Rda")

good_keepers <- features %>%
  dplyr::select(c(problem, method)) %>%
  distinct() %>%
  group_by(problem) %>%
  summarise(counter = n()) %>%
  ungroup() %>%
  filter(counter == 6) %>%
  pull(problem)

save(good_keepers, file = "data/good_keepers.Rda")
rm(features, train_test_ids)
