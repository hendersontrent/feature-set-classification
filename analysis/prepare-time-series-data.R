#------------------------------------------
# This script prepares the time series data
#
# NOTE: This script requires setup.R to have 
# been run first
#-----------------------------------------

#-------------------------------------
# Author: Trent Henderson, 2 June 2022
#-------------------------------------

# List all directories

directories <- list.dirs(path = "data/Univariate_arff", full.names = TRUE, recursive = TRUE)

# Remove non-folders

directories <- directories[!directories %in% c("data/Univariate_arff", "data/Univariate_arff/Pictures",
                                               "data/Univariate_arff/Pictures/fall_2018_datasets_crop_jpg")]

# Set up progress bar for {purrr}

pb <- dplyr::progress_estimated(length(directories))

# Get time-series data

TimeSeriesData <- directories %>%
  purrr::map_df(~ tidy_arff_files(x = .x))

save(TimeSeriesData, file = "data/TimeSeriesData.Rda")
