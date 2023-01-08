#-----------------------------------------
# This script analyses some specific cases
# pulled out from the normalised performance
# score graphic to understand the types of
# time series contained in each to aid 
# interpretation
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 06 January 2023
#-----------------------------------------

# Load raw time series

load("data/TimeSeriesData.Rda")

# Load classification results

load("data/outputs_z.Rda")

outputs_z_agg <- outputs_z %>%
  mutate(method = case_when(
    method == "tsfel" ~ "TSFEL",
    method == "kats"  ~ "Kats",
    TRUE              ~ method)) %>%
  group_by(problem, method) %>%
  summarise(avg = mean(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup()

# Define helper functions

#' Function to iterate and return top feature results
#' @param theprob \code{character} denoting the problem name
#' @return object of class \code{data.frame}
#' @author Trent Henderson
#'

iterate_top_features <- function(theprob){
  
  message(paste0("Doing ", theprob))
  load(paste0("data/feature-calcs/z-scored/", theprob ,".Rda"))
  
  the_top <- compute_top_features2(outs_z, 
                                   id_var = "id", 
                                   group_var = "group",
                                   num_features = 40, 
                                   method = "z-score",
                                   test_method = "svmLinear",
                                   use_balanced_accuracy = TRUE,
                                   use_k_fold = TRUE,
                                   num_folds = 10,
                                   use_empirical_null =  TRUE,
                                   null_testing_method = "ModelFreeShuffles",
                                   p_value_method = "gaussian",
                                   num_permutations = 1000,
                                   seed = 123)$ResultsTable %>%
    mutate(problem = theprob)
  
  return(the_top)
}

#-------------------------------------------------------
# INVESTIGATE WHY TSFRESH DOES WELL ON A SET OF PROBLEMS
#-------------------------------------------------------

# Time-series plots

tsfresh_probs <- c("UMD", "EthanolLevel", "ChlorineConcentration", "CricketY", "UWaveGestureLibraryAll")
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == tsfresh_probs[1], ], colour_by_split = FALSE)
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == tsfresh_probs[2], ], colour_by_split = FALSE)
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == tsfresh_probs[3], ], colour_by_split = FALSE)
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == tsfresh_probs[4], ], colour_by_split = FALSE)
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == tsfresh_probs[5], ], colour_by_split = FALSE)

# Top feature analysis

tsfresh_tops <- tsfresh_probs %>%
  purrr::map_dfr(~ iterate_top_features(theprob = .x))

save(tsfresh_tops, file = "data/case-studies/tsfresh_tops.Rda")

#---------------------------------------------------------
# INVESTIGATE WHY CATCH22 DOES POORLY ON A SET OF PROBLEMS
#---------------------------------------------------------

# Time-series plots

c22_bad_probs <- c("FordA", "Strawberry", "OSULeaf", "Wine")
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == c22_bad_probs[1], ], colour_by_split = FALSE)
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == c22_bad_probs[2], ], colour_by_split = FALSE)
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == c22_bad_probs[3], ], colour_by_split = FALSE)

# Top feature analysis

c22_bad_tops <- c22_bad_probs %>%
  purrr::map_dfr(~ iterate_top_features(theprob = .x))

save(c22_bad_tops, file = "data/case-studies/c22_bad_tops.Rda")

#------------------------------------------------------
# INVESTIGATE WHY KATS DOES POORLY ON A SET OF PROBLEMS
#------------------------------------------------------

# Time-series plots

kats_probs <- c("UWaveGestureLibraryZ", "GunPoint", "SyntheticControl")
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == kats_probs[1], ], colour_by_split = FALSE)
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == kats_probs[2], ], colour_by_split = FALSE)
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == kats_probs[3], ], colour_by_split = FALSE)

# Top feature analysis

kats_tops <- kats_probs %>%
  purrr::map_dfr(~ iterate_top_features(theprob = .x))

save(kats_tops, file = "data/case-studies/kats_tops.Rda")

#--------------------------------------------------------
# INVESTIGATE WHY FEASTS DOES POORLY ON A SET OF PROBLEMS
#--------------------------------------------------------

# Time-series plots

feasts_probs <- c("Wafer", "ProximalPhalanxOutlineCorrect", "FaceAll", "ACSF1", "HouseTwenty", "DodgerLoopWeekend")
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == feasts_probs[1], ], colour_by_split = FALSE)
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == feasts_probs[2], ], colour_by_split = FALSE)
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == feasts_probs[3], ], colour_by_split = FALSE)
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == feasts_probs[4], ], colour_by_split = FALSE)
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == feasts_probs[5], ], colour_by_split = FALSE)
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == feasts_probs[5], ], colour_by_split = FALSE)

# Top feature analysis

feasts_tops <- feasts_probs %>%
  purrr::map_dfr(~ iterate_top_features(theprob = .x))

save(feasts_tops, file = "data/case-studies/feasts_tops.Rda")

#-------------------------------------------------------
# INVESTIGATE WHY TSFEL DOES POORLY ON A SET OF PROBLEMS
#-------------------------------------------------------

# Time-series plots

tsfel_probs <- c("SemgHandGenderCh2", "SemgHandSubjectCh2", "CBF", "SemgHandMovementCh2", "Symbols")
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == tsfel_probs[1], ], colour_by_split = FALSE)
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == tsfel_probs[2], ], colour_by_split = FALSE)
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == tsfel_probs[3], ], colour_by_split = FALSE)
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == tsfel_probs[4], ], colour_by_split = FALSE)
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == tsfel_probs[5], ], colour_by_split = FALSE)

# Top feature analysis

tsfel_tops <- tsfel_probs %>%
  purrr::map_dfr(~ iterate_top_features(theprob = .x))

save(tsfel_tops, file = "data/case-studies/tsfel_tops.Rda")

#-------------------------------------------------------
# INVESTIGATE WHY CATCH22 DOES WELL ON A SET OF PROBLEMS
#-------------------------------------------------------

# Time-series plots

c22_good_probs <- c("FreezerSmallTrain", "FreezerRegularTrain", "Plane")
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == c22_good_probs[1], ], colour_by_split = FALSE)
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == c22_good_probs[2], ], colour_by_split = FALSE)
plot_all_ts(data = TimeSeriesData[TimeSeriesData$problem == c22_good_probs[3], ], colour_by_split = FALSE)

# Top feature analysis

c22_good_tops <- c22_good_probs %>%
  purrr::map_dfr(~ iterate_top_features(theprob = .x))

save(c22_good_tops, file = "data/case-studies/c22_good_tops.Rda")

# Larger "Plane" list

load("data/feature-calcs/z-scored/Plane.Rda")

c22_plane_top <- compute_top_features2(outs_z, 
                                       id_var = "id", 
                                       group_var = "group",
                                       num_features = 200, 
                                       method = "z-score",
                                       test_method = "svmLinear",
                                       use_balanced_accuracy = TRUE,
                                       use_k_fold = TRUE,
                                       num_folds = 10,
                                       use_empirical_null =  TRUE,
                                       null_testing_method = "ModelFreeShuffles",
                                       p_value_method = "gaussian",
                                       num_permutations = 1000,
                                       seed = 123)
