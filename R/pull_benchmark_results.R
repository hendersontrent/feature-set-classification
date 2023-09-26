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
  
  # Download results files and wrangle into correct format
  
  classifiers <- c("https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/1NN-DTW_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/Arsenal_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/BOSS_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/CIF_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/CNN_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/Catch22_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/DrCIF_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/EE_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/FreshPRINCE_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/HC1_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/HC2_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/Hydra-MR_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/Hydra_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/InceptionT_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/Mini-R_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/MrSQM_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/Multi-R_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/PF_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/RDST_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/RISE_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/RIST_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/ROCKET_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/RSF_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/RSTSF_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/ResNet_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/STC_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/STSF_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/ShapeDTW_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/Signatures_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/TDE_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/TS-CHIEF_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/TSF_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/TSFresh_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/WEASEL-D_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/WEASEL_ACC.csv",
                   "https://raw.githubusercontent.com/time-series-machine-learning/tsml-eval/main/results/classification/Univariate/cBOSS_ACC.csv")
  
  storage <- vector(mode = "list", length = length(classifiers))
  
  for(i in classifiers){
    temp <- read_csv(i)
    classifier <- gsub(".*/", "\\1", i)
    classifier <- gsub(".csv", "\\1", classifier)
    classifier <- gsub("_ACC", "\\1", classifier)
    
    temp <- temp %>%
      rename(problem = 1) %>%
      pivot_longer(cols = 2:ncol(temp), names_to = "resample", values_to = "accuracy") %>%
      mutate(resample = as.numeric(resample),
             resample = resample + 1) %>%
      mutate(method = classifier)
    
    storage[[i]] <- temp
  }
  
  storage <- do.call("rbind", storage)
  rownames(storage) <- NULL
  return(storage)
}
