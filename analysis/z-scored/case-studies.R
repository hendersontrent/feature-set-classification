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

# Top feature summaries

head(case_study_tops[[1]]$SummaryStatistics %>% arrange(-.mean), n = 10)
head(case_study_tops[[2]]$SummaryStatistics %>% arrange(-.mean), n = 10)
head(case_study_tops[[3]]$SummaryStatistics %>% arrange(-.mean), n = 10)
head(case_study_tops[[4]]$SummaryStatistics %>% arrange(-.mean), n = 10)

# Sample sizes

source("analysis/summarise-problems.R")

problem_summaries <- problem_summaries %>%
  filter(problem %in% case_studies)

# Time-series length

load("data/TimeSeriesData.Rda")

lengths <- TimeSeriesData %>%
  group_by(problem) %>%
  summarise(samples = max(timepoint)) %>%
  ungroup() %>%
  filter(problem %in% case_studies)

problem_summaries <- problem_summaries %>%
  inner_join(lengths)

rm(lengths)

#-------------------------------------------------------------------------------
#---------------------------- FFT AND QUANTILES CASES --------------------------
#-------------------------------------------------------------------------------

#-----------------------------
# FFT -- ChlorineConcentration
# outperformed mean of feature 
# sets (check spectra)
#-----------------------------

# Filter data

ChlorineConcentration <- TimeSeriesData %>%
  filter(problem == "ChlorineConcentration")

# Draw plots

spectrum(ChlorineConcentration %>% filter(id == "1_ChlorineConcentration") %>% pull(values))
spectrum(ChlorineConcentration %>% filter(id == "5_ChlorineConcentration") %>% pull(values))
spectrum(ChlorineConcentration %>% filter(id == "2_ChlorineConcentration") %>% pull(values))

#--------------------------
# Quantiles -- EthanolLevel
# outperformed mean of
# feature sets (check 
# distributions)
#--------------------------

# Filter data

EthanolLevel <- TimeSeriesData %>%
  filter(problem == "EthanolLevel")

rm(TimeSeriesData)

# Draw plots

EthanolLevel %>%
  ggplot(aes(x = values)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8) +
  labs(x = "Value",
       y = "Density") +
  theme_bw() +
  facet_wrap(~target)
