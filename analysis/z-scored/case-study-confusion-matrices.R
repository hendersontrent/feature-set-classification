#----------------------------------------------
# This script sets out to analyse the confusion
# matrices for the case study models
#----------------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 19 September 2022
#-------------------------------------------

# Load in data and summarise to just problem, ID, and train-test set indicator as I didn't bind initially

load("data/TimeSeriesData.Rda")

train_test_ids <- TimeSeriesData %>%
  dplyr::select(c(problem, id, set_split)) %>%
  distinct() %>% 
  filter(problem %in% c("Plane", "PhalangesOutlinesCorrect", "FreezerSmallTrain"))

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

data_files <- c("data/feature-calcs/z-scored/Plane.Rda",
                "data/feature-calcs/z-scored/PhalangesOutlinesCorrect.Rda",
                "data/feature-calcs/z-scored/FreezerSmallTrain.Rda")

conf_mats <- data_files %>%
  purrr::map(~ get_confusion_matrices(theproblem = .x, tt_labels = train_test_ids, set = FALSE))

conf_mats_set <- data_files %>%
  purrr::map(~ get_confusion_matrices(theproblem = .x, tt_labels = train_test_ids, set = TRUE))

#---------------- Confusion matrix analysis -----------------

#------
# Plane
#------

# Each individual confusion matrix for comparison

conf_mats_set[[1]]$catch22$Resample_1
conf_mats_set[[1]]$feasts$Resample_1
conf_mats_set[[1]]$tsfeatures$Resample_1
conf_mats_set[[1]]$tsfresh$Resample_1
conf_mats_set[[1]]$tsfel$Resample_1
conf_mats_set[[1]]$kats$Resample_1

#-------------------------
# PhalangesOutlinesCorrect
#-------------------------

conf_mats_set[[2]]$catch22$Resample_1
conf_mats_set[[2]]$feasts$Resample_1
conf_mats_set[[2]]$tsfeatures$Resample_1
conf_mats_set[[2]]$tsfresh$Resample_1
conf_mats_set[[2]]$tsfel$Resample_1
conf_mats_set[[2]]$kats$Resample_1

#------------------
# FreezerSmallTrain
#------------------

conf_mats_set[[3]]$catch22$Resample_1
conf_mats_set[[3]]$feasts$Resample_1
conf_mats_set[[3]]$tsfeatures$Resample_1
conf_mats_set[[3]]$tsfresh$Resample_1
conf_mats_set[[3]]$tsfel$Resample_1
conf_mats_set[[3]]$kats$Resample_1

#---------------- Follow-up pairwise analyses -----------------

#------
# Plane
#------

# Set up binary groups

load("data/feature-calcs/z-scored/Plane.Rda")
plane_binary <- outs_z
rm(outs_z)

plane_binary <- plane_binary %>%
  mutate(group = as.factor(group)) %>%
  mutate(group_1 = ifelse(group == "6", "Group 6", "Everything else"),
         group_2 = ifelse(group == "3", "Group 3", "Everything else")) %>%
  mutate(group_1 = as.factor(group_1),
         group_2 = as.factor(group_2)) %>%
  dplyr::select(-c(group))

# Compute top features

plane_bin_top_1 <- compute_top_features(plane_binary[plane_binary$method == "catch22", ], 
                                        id_var = "id", 
                                        group_var = "group_1",
                                        num_features = 22, 
                                        method = "z-score",
                                        test_method = "svmLinear",
                                        use_balanced_accuracy = TRUE,
                                        use_k_fold = TRUE,
                                        num_folds = 10,
                                        use_empirical_null =  TRUE,
                                        null_testing_method = "ModelFreeShuffles",
                                        p_value_method = "gaussian",
                                        num_permutations = 1e4,
                                        seed = 123)

View(plane_bin_top_1$ResultsTable)

plane_bin_top_1_all <- compute_top_features(plane_binary, 
                                            id_var = "id", 
                                            group_var = "group_1",
                                            num_features = 22, 
                                            method = "z-score",
                                            test_method = "svmLinear",
                                            use_balanced_accuracy = TRUE,
                                            use_k_fold = TRUE,
                                            num_folds = 10,
                                            use_empirical_null =  TRUE,
                                            null_testing_method = "ModelFreeShuffles",
                                            p_value_method = "gaussian",
                                            num_permutations = 1e4,
                                            seed = 123)

View(plane_bin_top_1_all$ResultsTable)

plane_bin_top_2 <- compute_top_features(plane_binary[plane_binary$method == "catch22", ], 
                                        id_var = "id", 
                                        group_var = "group_2",
                                        num_features = 22, 
                                        method = "z-score",
                                        test_method = "svmLinear",
                                        use_balanced_accuracy = TRUE,
                                        use_k_fold = TRUE,
                                        num_folds = 10,
                                        use_empirical_null =  TRUE,
                                        null_testing_method = "ModelFreeShuffles",
                                        p_value_method = "gaussian",
                                        num_permutations = 1e4,
                                        seed = 123)

View(plane_bin_top_2$ResultsTable)

plane_bin_top_2_all <- compute_top_features(plane_binary, 
                                            id_var = "id", 
                                            group_var = "group_2",
                                            num_features = 22, 
                                            method = "z-score",
                                            test_method = "svmLinear",
                                            use_balanced_accuracy = TRUE,
                                            use_k_fold = TRUE,
                                            num_folds = 10,
                                            use_empirical_null =  TRUE,
                                            null_testing_method = "ModelFreeShuffles",
                                            p_value_method = "gaussian",
                                            num_permutations = 1e4,
                                            seed = 123)

View(plane_bin_top_2_all$ResultsTable)
