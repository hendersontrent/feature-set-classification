#----------------------------------------------
# This script sets out to analyse the confusion
# matrices for the case study models
#----------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 3 September 2022
#------------------------------------------

# Load in data and summarise to just problem, ID, and train-test set indicator as I didn't bind initially

load("data/TimeSeriesData.Rda")

train_test_ids <- TimeSeriesData %>%
  dplyr::select(c(problem, id, set_split)) %>%
  distinct() %>% 
  filter(problem %in% c("Coffee", "ProximalPhalanxOutlineAgeGroup", "Plane"))

rm(TimeSeriesData) # Clean up environment as dataframe is large

#---------------- Confusion matrix calculations -----------------

#' Function to map classification performance calculations over datasets/problems
#' @param theproblem filepath to the feature data
#' @param tt_labels the dataframe containing train-test labels
#' @param set Boolean whether to fit by set or not
#' @param set_filt string name of individual feature set to calculate results for. Defaults to \code{NULL} for no usage
#' @returns an object of class dataframe
#' @author Trent Henderson
#' 

get_confusion_matrices <- function(theproblem, tt_labels, set = TRUE, set_filt = NULL){
  
  files <- list.files("data/feature-calcs/z-scored", full.names = TRUE, pattern = "\\.Rda")
  message(paste0("Doing problem ", match(theproblem, files), "/", length(files)))
  load(theproblem)
  problem_name <- gsub(".*/", "\\1", theproblem)
  problem_name <- gsub(".Rda", "\\1", problem_name)
  
  # Join in train-test indicator
  
  outs_z <- outs_z %>%
    filter(names != "DN_Mean") %>% # Just last ditch case in case it slips through
    filter(names != "DN_Spread_Std") %>% # Just last ditch case in case it slips through
    inner_join(tt_labels, by = c("id" = "id")) %>%
    dplyr::select(-c(problem))
  
  if(!is.null(set_filt)){
    outs_z <- outs_z %>%
      filter(method == set_filt)
  }
  
  # Fit multi-feature classifiers by feature set
  
  results <- fit_multi_feature_classifier_tt(outs_z, 
                                             id_var = "id", 
                                             group_var = "group",
                                             by_set = set, 
                                             test_method = "svmLinear", 
                                             use_balanced_accuracy = TRUE,
                                             use_k_fold = TRUE, 
                                             num_folds = 10, 
                                             num_resamples = 1,
                                             problem_name = problem_name,
                                             conf_mat = TRUE)
  
  return(results)
}

data_files <- c("data/feature-calcs/z-scored/Coffee.Rda", "data/feature-calcs/z-scored/ProximalPhalanxOutlineAgeGroup.Rda",
                "data/feature-calcs/z-scored/Plane.Rda")

conf_mats <- data_files %>%
  purrr::map(~ get_confusion_matrices(theproblem = .x, tt_labels = train_test_ids, set = FALSE))

conf_mats_set <- data_files %>%
  purrr::map(~ get_confusion_matrices(theproblem = .x, tt_labels = train_test_ids, set = TRUE))

#---------------- Confusion matrix analysis -----------------

#-------
# Coffee
#-------

conf_mats[[1]]$Resample_1

#-------------------------------
# ProximalPhalanxOutlineAgeGroup
#-------------------------------

conf_mats[[2]]$Resample_1

# Each individual confusion matrix for comparison

for(i in 1:length(conf_mats_set[[2]])){
  print(names(conf_mats_set[[2]][i]))
  print(conf_mats_set[[2]][[i]]$Resample_1$overall)
}

conf_mats_set[[2]]$tsfresh$Resample_1

#------
# Plane
#------

conf_mats[[3]]$Resample_1

# Each individual confusion matrix for comparison

conf_mats_set[[3]]$catch22$Resample_1
conf_mats_set[[3]]$feasts$Resample_1
conf_mats_set[[3]]$tsfeatures$Resample_1
conf_mats_set[[3]]$tsfresh$Resample_1
conf_mats_set[[3]]$tsfel$Resample_1
conf_mats_set[[3]]$kats$Resample_1
