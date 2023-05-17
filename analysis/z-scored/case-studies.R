#--------------------------------------
# This script sets out to analyse some
# case study problems to more deeply
# understand why a given feature set
# performs well or poorly on it
#--------------------------------------

#-------------------------------------
# Author: Trent Henderson, 15 May 2023
#-------------------------------------

# Define list of problems

case_studies <- c("PigArtPressure", "PigCVP", "DistalPhalanxOutlineCorrect",
                  "MiddlePhalanxTW", "MiddlePhalanxOutlineAgeGroup", "Yoga",
                  "RefrigerationDevices", "FreezerSmallTrain", "FreezerRegularTrain")

# Calculate top features for each case study

case_study_tops <- vector(mode = "list", length = length(case_studies))

for(i in case_studies){
  
  message(paste0("Doing ", i))
  load(paste0("data/feature-calcs/z-scored/", i, ".Rda"))
  
  outs <- outs %>%
    filter(names %ni% c("DN_Mean", "DN_Spread_Std")) # Remove mean and variance from catch22
  
  outs <- structure(list(outs), class = "feature_calculations")
  
  top_features <- try(compute_top_features(outs, 
                                           num_features = 50, 
                                           method = "z-score",
                                           test_method = "svmLinear",
                                           use_balanced_accuracy = FALSE,
                                           use_k_fold = TRUE,
                                           num_folds = 10,
                                           use_empirical_null =  TRUE,
                                           null_testing_method = "ModelFreeShuffles",
                                           p_value_method = "empirical",
                                           num_permutations = 100,
                                           seed = 123))
  
  if(inherits(top_features, "try-error") == TRUE){
    top_features <- NULL
  }
  
  case_study_tops[[match(i, case_studies)]] <- top_features
}

names(case_study_tops) <- case_studies
rm(case_studies, top_features, i)
save(case_study_tops, file = "data/case-studies/case_study_tops.Rda")
