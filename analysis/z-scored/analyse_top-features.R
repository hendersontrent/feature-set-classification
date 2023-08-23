#-----------------------------------------
# This script sets out to find the top
# features for each problem
#-----------------------------------------

#----------------------------------------
# Author: Trent Henderson, 21 August 2023
#----------------------------------------

# Load results and bind

data_files <- list.files("data/individual-feature-classifiers", full.names = TRUE, pattern = "\\.Rda")
individual_feats <- vector(mode = "list", length = length(data_files))

for(d in data_files){
  load(d)
  individual_feats[[match(d, data_files)]] <- outputs
}

rm(data_files)

#--------------- Calculate top features --------------

# Run calculations

top_features <- vector(mode = "list", length = length(individual_feats))

for(i in 1:length(individual_feats)){
  message(paste0("Doing ", match(i, 1:length(top_features)), "/", length(top_features)))
  comps <- compare_features(individual_feats[[i]], by_set = FALSE, hypothesis = "null")
  comps$problem <- unique(individual_feats[[i]]$ClassificationResults$problem)
  top_features[[i]] <- comps
}

top_features <- do.call("rbind", top_features)
save(top_features, file = "data/top_features.Rda")

# Find top N for each problem and manually add in qualitative groups based on "analysis/z-scored/normalised-performance-score.R"

N <- 40

top_n_feats <- top_features %>%
  group_by(problem) %>%
  slice_max(feature_mean, n = N) %>%
  ungroup() %>%
  mutate(qual_label = case_when(
          problem == "MixedShapesSmallTrain"    ~ "tsfresh wins",
          problem == "MixedShapesRegularTrain"  ~ "tsfresh wins",
          problem == "FaceFour"                 ~ "tsfresh wins",
          problem == "UMD"                      ~ "tsfresh wins",
          problem == "OliveOil"                 ~ "tsfresh wins",
          problem == "InsectWingbeatSound"      ~ "tsfresh wins",
          problem == "ChlorineConcentration"    ~ "tsfresh wins",
          problem == "EthanolLevel"             ~ "tsfresh wins",
          problem == "FaceAll"                  ~ "tsfresh wins",
          problem == "CricketX"                 ~ "tsfresh wins",
          problem == "CricketZ"                 ~ "tsfresh wins",
          problem == "FacesUCR"                 ~ "tsfresh wins",
          problem == "CricketY"                 ~ "tsfresh wins",
          problem == "PhalangesOutlinesCorrect" ~ "tsfresh wins",
          problem == "Fish"                     ~ "tsfresh wins",
          problem == "Beef"                     ~ "tsfresh wins",
          problem == "Ham"                      ~ "tsfresh wins",
          problem == "PowerCons"                ~ "tsfresh wins",
          problem == "Meat"                     ~ "tsfresh wins",
          problem == "PigCVP"                   ~ "TSFEL wins",
          problem == "PigArtPressure"           ~ "TSFEL wins",
          problem == "Yoga"                     ~ "Kats wins",
          problem == "FreezerSmallTrain"        ~ "catch22 wins",
          problem == "FreezerRegularTrain"      ~ "catch22 wins",
          problem == "MiddlePhalanxTW"          ~ "feasts does well",
          problem == "Phoneme"                  ~ "tsfeatures wins",
          problem == "Car"                      ~ "tsfeatures wins",
          TRUE                                  ~ "Not a focus")) %>%
  filter(qual_label != "Not a focus") %>%
  group_by(qual_label, names, feature_set) %>%
  summarise(counter = n()) %>%
  ungroup() %>%
  mutate(num_probs_in_qual = case_when(
          qual_label == "tsfresh wins"     ~ 19,
          qual_label == "TSFEL wins"       ~ 2,
          qual_label == "Kats wins"        ~ 1,
          qual_label == "feasts does well" ~ 1,
          qual_label == "tsfeatures wins"  ~ 2,
          qual_label == "catch22 wins"     ~ 2)) %>%
  mutate(props = counter / num_probs_in_qual)
