#------------------------------------------
# This script sets out to produce analysis
# to understand why tsfresh and TSFEL are
# so represented in the head-to-head analyses,
# with the hypothesis that it's due to phase-
# aligned problem data
#
# NOTE: This script requires setup.R and
# analysis/compute-features.R and
# analysis/fit-classifiers.R to have been 
# run first
#-----------------------------------------

#--------------------------------------
# Author: Trent Henderson, 25 July 2022
#--------------------------------------

# Load classification results

load("data/outputs.Rda")

outputs <- outputs %>%
  mutate(method = case_when(
    method == "tsfel" ~ "TSFEL",
    method == "kats"  ~ "Kats",
    TRUE              ~ method))

# Get list of problems where tsfresh and TSFEL are the top performing set

prob_list <- outputs %>%
  mutate(balanced_accuracy = balanced_accuracy * 100) %>%
  group_by(problem, method) %>%
  summarise(best_balanced_accuracy_mean = mean(balanced_accuracy, na.rm = TRUE),
            best_balanced_accuracy_sd = sd(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(problem) %>%
  mutate(ranker = dense_rank(-best_balanced_accuracy_mean)) %>%
  ungroup() %>%
  filter(ranker == 1) %>%
  filter(method %in% c("tsfresh", "TSFEL")) %>%
  dplyr::select(c(problem, method))

#---------------------- Do analysis by problem -----------------------

#' Function to find top features for the top sets by problem
#' @param setdata dataframe of set and feature set name combinations
#' @param theproblem filepath of problem feature data to operate on
#' @param allfiles vector of all problem feature data filepaths
#' @return object of class \code{data.frame}
#' @author Trent Henderson
#' 

find_top_features <- function(setdata, theproblem, allfiles){
  
  message(paste0("Doing problem ", match(theproblem, allfiles), "/", length(allfiles)))
  load(theproblem)
  problem_name <- gsub(".*/", "\\1", theproblem)
  problem_name <- gsub(".Rda", "\\1", problem_name)
  
  tmp_set <- setdata %>%
    filter(problem == problem_name)
  
  outs <- outs %>%
    filter(method == tmp_set$method)
  
  # Calculate features
  
  outputs <- compute_top_features(data = outs, 
                                  id_var = "id", 
                                  group_var = "group",
                                  num_features = 20, 
                                  normalise_violin_plots = FALSE,
                                  cor_method = "pearson",
                                  test_method = "svmLinear",
                                  clust_method = "average",
                                  use_balanced_accuracy = TRUE,
                                  use_k_fold = TRUE,
                                  num_folds = 10,
                                  use_empirical_null =  TRUE,
                                  null_testing_method = "ModelFreeShuffles",
                                  p_value_method = "gaussian",
                                  num_permutations = 100,
                                  pool_empirical_null = FALSE,
                                  seed = 123)
  
  ResultsTable <- outputs$ResultsTable %>%
    mutate(problem = problem_name)
  
  return(ResultsTable)
}

find_top_features_safe <- purrr::possibly(find_top_features, otherwise = NULL)
probs <- paste0("data/feature-calcs/", unique(prob_list$problem), ".Rda") # Generate data file list as they are independent

tops <- probs %>%
  purrr::map_df(~ find_top_features_safe(setdata = prob_list, theproblem = .x, allfiles = probs))

save(tops, file = "data/non-z-scored-tops.Rda")

#---------------------- Produce summary plot -----------------------

# Plot

tops %>%
  mutate(feature = gsub("tsfel", "TSFEL", feature),
         feature = gsub("tsfresh", "TSFRESH", feature)) %>%
  group_by(feature) %>%
  summarise(counter = n()) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(feature, -counter), y = counter)) +
  geom_bar(stat = "identity") +
  labs(title = paste0("Frequency of top features across", length(probs), " problems where tsfresh/TSFEL was the best"),
       x = "Feature",
       y = "Frequency") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

# Numerical summary of top 10 by set

totals <- prob_list %>%
  group_by(method) %>%
  summarise(total_problems = n()) %>%
  ungroup()

frequencies <- tops %>%
  mutate(feature = gsub("tsfel", "TSFEL", feature)) %>%
  group_by(feature) %>%
  summarise(occurrences = n()) %>%
  ungroup() %>%
  mutate(method = gsub("_.*", "\\1", feature)) %>%
  inner_join(totals, by = c("method" = "method")) %>%
  mutate(prop_occurrences = occurrences / total_problems) %>%
  arrange(-prop_occurrences)
