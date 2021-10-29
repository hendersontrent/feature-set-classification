#------------------------------------------
# This script sets out to load all the 
# packages and folders necessary for the 
# project. It requires http://www.timeseriesclassification.com/Downloads/Archives/Univariate2018_arff.zip
# to be put into the data/ folder
#
# NOTE: This script requires setup.R to
# have been run first
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 29 October 2021
#-----------------------------------------

#' Function to load and process files into a tidy dataframe
#' 
#' @return an object of class dataframe
#' @author Trent Henderson
#' 

tidy_arff_files <- function(){
  
  directories <- list.dirs(path = "data/Univariate_arff", full.names = TRUE, recursive = TRUE)
  
  # Remove non-folders
  
  directories <- directories[!directories %in% c("data/Univariate_arff")]
  
  #------------ Parse problems ---------
  
  TimeSeriesData <- list()
  
  for(d in directories){
    
    tryCatch({
      
      #-----------------------------
      # Grab the train and test data
      #-----------------------------
      
      # Shorten filepath to problem name
      
      probName <- gsub(".*\\/", "\\1", d)
      
      # Retrieve TRAIN and TEST files
      
      train <- foreign::read.arff(paste0(d,"/",probName,"_TRAIN.arff")) %>%
        mutate(id = row_number()) %>%
        mutate(set_split = "Train")
      
      themax <- max(train$id) # To add in test set to avoid duplicate IDs
      
      test <- foreign::read.arff(paste0(d,"/",probName,"_TEST.arff")) %>%
        mutate(id = row_number()+themax) %>% # Adjust relative to train set to stop double-ups
        mutate(set_split = "Test")
    
    }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  
  #------------ Save output ------------
  
  save(TimeSeriesData, file = "data/TimeSeriesData.Rda")
}
