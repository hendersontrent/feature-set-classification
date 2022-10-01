#--------------------------------------
# This script sets out to pull existing
# classifier results for all the 
# problems for comparison
#--------------------------------------

#-------------------------------------
# Author: Trent Henderson, 19 May 2022
#-------------------------------------

#' Function to pull results and wrangle into tidy format
#' @return object of class dataframe
#' @author Trent Henderson
#' 

pull_benchmark_results <- function(){
  
  # Download results files
  
  url <- "https://www.timeseriesclassification.com/results/tsml/tsmlUnivariateResults.zip"
  temp <- tempfile()
  download.file(url, temp, mode = "wb")
  
  paths <- c("ResultsByClassifier/BOSS_TESTFOLDS.csv", "ResultsByClassifier/cBOSS_TESTFOLDS.csv",
             "ResultsByClassifier/HIVE-COTEv1_0_TESTFOLDS.csv", "ResultsByClassifier/InceptionTime_TESTFOLDS.csv",
             "ResultsByClassifier/ProximityForest_TESTFOLDS.csv", "ResultsByClassifier/ResNet_TESTFOLDS.csv",
             "ResultsByClassifier/RISE_TESTFOLDS.csv", "ResultsByClassifier/ROCKET_TESTFOLDS.csv",
             "ResultsByClassifier/S-BOSS_TESTFOLDS.csv", "ResultsByClassifier/STC_TESTFOLDS.csv",
             "ResultsByClassifier/TS-CHIEF_TESTFOLDS.csv", "ResultsByClassifier/TSF_TESTFOLDS.csv",
             "ResultsByClassifier/WEASEL_TESTFOLDS.csv")
  
  # Extract .csv files and tidy up
  
  accs <- list()
  
  for(i in paths){
    
    themethod <- gsub("ResultsByClassifier/", "\\1", i)
    themethod <- gsub("_TESTFOLDS.csv", "\\1", themethod)
    
    tmp <- readr::read_csv(unz(temp, filename = i)) %>%
      rename(problem = "folds:") %>%
      pivot_longer(cols = "0":"29", names_to = "resample", values_to = "accuracy") %>%
      mutate(method = themethod)
    
    accs[[i]] <- tmp
  }
  
  accs <- do.call("rbind", accs)
  rownames(accs) <- NULL
  return(accs)
}
