#--------------------------------------
# This script sets out to pull existing
# classifier results for all the 
# problems for comparison
#--------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 12 October 2023
#-----------------------------------------

#' Function to pull results and wrangle into tidy format
#' @return \code{data.frame} in tidy format of results
#' @author Trent Henderson
#' 

pull_benchmark_results <- function(){
  
  # Download results files and wrangle into correct format
  
  storage <- tempfile()
  download.file(url = "https://www.timeseriesclassification.com/AllSplits.zip", storage, mode = "wb")
  
  # Extract .csv files and tidy up
  
  classifiers <- c("BoP.csv", "BOSS.csv", "CID_DTW.csv", "DD_DTW.csv",
                   "DDTW.csv", "DTD_C.csv", "DTW_F.csv", "EE.csv",
                   "Flat-COTE.csv", "FS.csv", "HIVE-COTE.csv", "LPS.csv",
                   "LS.csv", "MSM.csv", "RotF.csv", "SAXVSM.csv",
                   "ST.csv", "TSBF.csv", "TSF.csv", "TWE.csv", 
                   "WDTW.csv")
  
  classifiers <- paste0("AllSplits/", classifiers)
  accs <- list()
  
  for(i in classifiers){
    
    themethod <- gsub("AllSplits/", "\\1", i)
    themethod <- gsub(".csv", "\\1", themethod)
    
    tmp <- read.csv(unz(storage, filename = i), header = FALSE) %>%
      rename(problem = V1) %>%
      pivot_longer(cols = "V2":"V101", names_to = "resample", values_to = "accuracy") %>%
      mutate(method = themethod,
             resample = gsub("V", "\\1", resample),
             resample = as.integer(resample),
             resample = resample - 1) # To account for column number issues
    
    accs[[i]] <- tmp
  }
  
  accs <- do.call("rbind", accs)
  rownames(accs) <- NULL
  return(accs)
}
