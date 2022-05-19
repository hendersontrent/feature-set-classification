#---------------------------------------
# This script sets out to load all the 
# packages and folders necessary for the 
# project. It requires 
# http://www.timeseriesclassification.com/Downloads/Archives/Univariate2018_arff.zip
# to be put into the data/ folder
#
# NOTE: This script requires setup.R to
# have been run first
#---------------------------------------

#-------------------------------------
# Author: Trent Henderson, 06 May 2022
#-------------------------------------

# List all directories

directories <- list.dirs(path = "data/Univariate_arff", full.names = TRUE, recursive = TRUE)

# Remove non-folders

directories <- directories[!directories %in% c("data/Univariate_arff", "data/Univariate_arff/Pictures",
                                               "data/Univariate_arff/Pictures/fall_2018_datasets_crop_jpg")]

# Set up progress bar for {purrr}

pb <- dplyr::progress_estimated(length(directories))

#------------------ Processing function -----------------

#' Function to load and process files into a single tidy dataframe
#' @param x the directory to operate on
#' @return an object of class dataframe
#' @author Trent Henderson
#' 

tidy_arff_files <- function(x){
  
  # Print {purrr} iteration progress updates in the console
  
  pb$tick()$print()
  
  #-----------------------------
  # Grab the train and test data
  #-----------------------------
  
  # Shorten filepath to problem name
  
  probName <- gsub(".*\\/", "\\1", x)
  
  # Retrieve TRAIN and TEST files
  
  train <- foreign::read.arff(paste0(x, "/", probName, "_TRAIN.arff")) %>%
    mutate(id = row_number()) %>%
    mutate(set_split = "Train")
  
  themax <- max(train$id) # To add in test set to avoid duplicate IDs
  
  test <- foreign::read.arff(paste0(x, "/", probName, "_TEST.arff")) %>%
    mutate(id = row_number() + themax) %>% # Adjust relative to train set to stop double-ups
    mutate(set_split = "Test")
  
  #----------------------------
  # Wrangle data to long format
  #----------------------------
  
  # Train
  
  thecolstr <- colnames(train)
  keepcolstr <- thecolstr[!thecolstr %in% c("target", "id", "set_split")]
  
  train2 <- train %>%
    mutate(problem = probName) %>%
    pivot_longer(cols = all_of(keepcolstr), names_to = "timepoint", values_to = "values") %>%
    mutate(timepoint = as.numeric(gsub(".*?([0-9]+).*", "\\1", timepoint)))
  
  # Test
  
  thecolste <- colnames(test)
  keepcolste <- thecolste[!thecolste %in% c("target", "id", "set_split")]
  
  test2 <- test %>%
    mutate(problem = probName) %>%
    pivot_longer(cols = all_of(keepcolste), names_to = "timepoint", values_to = "values") %>%
    mutate(timepoint = as.numeric(gsub(".*?([0-9]+).*", "\\1", timepoint)))
  
  #------------
  # Merge files
  #------------
  
  tmp <- bind_rows(train2, test2) %>%
    mutate(id = paste0(id, "_", problem))
  
  return(tmp)
}
