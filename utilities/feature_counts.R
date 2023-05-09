#----------------------------------------------
# This script calculates the number of features
# calculated per problem and measures it against
# train size of each
#
# NOTE: This script requires setup.R and
# analysis/summarise_problems.R to have been
# run first
#----------------------------------------------

#------------------------------------
# Author: Trent Henderson, 9 May 2022
#------------------------------------

files <- list.files("data/feature-calcs/z-scored", full.names = TRUE, pattern = "\\.Rda")
features <- vector(mode = "list", length = length(files))

for(i in files){
  load(i)
  problem <- gsub("data/feature-calcs/z-scored/", "\\1", i)
  outs$problem <- gsub(".Rda", "\\1", problem)
  features[[match(i, files)]] <- outs
}

features <- do.call("rbind", features)

# Count number of total features per problem

counts <- features %>%
  mutate(feature = paste0(method, "_", names)) %>%
  dplyr::select(c(problem, feature)) %>%
  distinct() %>%
  group_by(problem) %>%
  summarise(counter = n()) %>%
  ungroup()

# Find number of problems with train samples < counts

small_problems <- problem_summaries %>%
  filter(Train < min(counts$counter))
