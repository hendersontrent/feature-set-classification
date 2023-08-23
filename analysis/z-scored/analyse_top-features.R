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

top_features <- vector(mode = "list", length = length(individual_feats))

for(i in 1:length(individual_feats)){
  message(paste0("Doing ", match(i, 1:length(top_features)), "/", length(top_features)))
  comps <- compare_features(individual_feats[[i]], by_set = FALSE, hypothesis = "null")
  comps$problem <- unique(individual_feats[[i]]$ClassificationResults$problem)
  top_features[[i]] <- comps
}

top_features <- do.call("rbind", top_features)
save(top_features, file = "data/top_features.Rda")
