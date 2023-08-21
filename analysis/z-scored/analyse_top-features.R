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
  individual_feats[[d]] <- outputs
}

individual_feats <- do.call("rbind", individual_feats)
rm(data_files)

#--------------- Calculate top features --------------


