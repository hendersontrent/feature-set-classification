#-------------------------------------------
# This script sets out to analyse some case 
# study problems to understand why a given 
# feature set performs well or poorly on it
#-------------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 16 September 2023
#-------------------------------------------

# Define list of problems

case_studies <- c("PigCVP", "Yoga", "MixedShapesRegularTrain", "Phoneme")

# Calculate top features for each case study

case_study_tops <- vector(mode = "list", length = length(case_studies))

for(i in case_studies){
  load(paste0("data/feature-calcs/z-scored/", i, ".Rda"))
  outs <- outs %>% rename(feature_set = method)
  outs <- structure(list(outs), class = "feature_calculations")
  top_features <- tsfeature_classifier(data = outs, classifier = NULL, train_size = 0.75, n_resamples = 10, by_set = FALSE, seed = 123)
  case_study_tops[[match(i, case_studies)]] <- top_features
}

# Clean up environment

names(case_study_tops) <- case_studies
rm(case_studies, top_features, i)

# Calculate summary values

case_study_tops[[1]]$SummaryStatistics <- calculate_interval(case_study_tops[[1]], metric = "accuracy", by_set = FALSE)
case_study_tops[[2]]$SummaryStatistics <- calculate_interval(case_study_tops[[2]], metric = "accuracy", by_set = FALSE)
case_study_tops[[3]]$SummaryStatistics <- calculate_interval(case_study_tops[[3]], metric = "accuracy", by_set = FALSE)
case_study_tops[[4]]$SummaryStatistics <- calculate_interval(case_study_tops[[4]], metric = "accuracy", by_set = FALSE)

# Save

save(case_study_tops, file = "data/case-studies/case_study_tops.Rda")

#--------------- Investigate each case study --------------

head(case_study_tops[[1]]$SummaryStatistics %>% arrange(-.mean), n = 10)
head(case_study_tops[[2]]$SummaryStatistics %>% arrange(-.mean), n = 10)
head(case_study_tops[[3]]$SummaryStatistics %>% arrange(-.mean), n = 10)
head(case_study_tops[[4]]$SummaryStatistics %>% arrange(-.mean), n = 10)
