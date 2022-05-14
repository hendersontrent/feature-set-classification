#------------------------------------------
# This script sets out to produce analysis
# of classification performance for sets
# versus all features at once
#
# NOTE: This script requires setup.R and
# analysis/compute-features.R and
# analysis/fit-classifiers.R to have been 
# run first
#-----------------------------------------

#-------------------------------------
# Author: Trent Henderson, 14 May 2022
#-------------------------------------

# Load classification results

load("data/outputs.Rda")
load("data/outputs_aggregate.Rda")

# Remove NULL entries that errored

outputs_filtered <- outputs[!sapply(outputs, is.null)]
outputs_aggregate_filtered <- outputs_aggregate[!sapply(outputs_aggregate, is.null)]
rm(outputs, outputs_aggregate)

#' Pull only main results for every problem and feature set
#' @param results the list containing classification results
#' @param x the index of the problem to get results for
#' @returns an object of class dataframe
#' @author Trent Henderson
#' 

pull_main_models <- function(results, x){
  
  # Filter list
  
  tmp <- results[[x]]
  
  # Extract relevant dataframe and filter to main models
  
  tmp <- tmp$RawClassificationResults %>%
    filter(category == "Main") %>%
    mutate(problem = names(results)[[x]])
  
  return(tmp)
}

# Run the function

main_models <- 1:length(outputs_filtered) %>%
  purrr::map_df(~ pull_main_models(results = outputs_filtered, x = .x)) %>%
  mutate(accuracy = accuracy * 100,
         balanced_accuracy = balanced_accuracy * 100,
         accuracy_sd = accuracy_sd * 100,
         balanced_accuracy_sd = balanced_accuracy_sd * 100) %>%
  group_by(problem) %>%
  mutate(ranker = dense_rank(-balanced_accuracy)) %>%
  ungroup() %>%
  filter(ranker == 1) %>%
  dplyr::select(problem, method, balanced_accuracy, balanced_accuracy_sd) %>%
  rename(method_set = method)

main_models_aggregate <- 1:length(outputs_aggregate_filtered) %>%
  purrr::map_df(~ pull_main_models(results = outputs_aggregate_filtered, x = .x)) %>%
  mutate(accuracy = accuracy * 100,
         balanced_accuracy = balanced_accuracy * 100,
         accuracy_sd = accuracy_sd * 100,
         balanced_accuracy_sd = balanced_accuracy_sd * 100,
         method = "All Features") %>%
  dplyr::select(problem, method, balanced_accuracy, balanced_accuracy_sd) %>%
  rename(balanced_accuracy_all = balanced_accuracy,
         balanced_accuracy_sd_all = balanced_accuracy_sd)

# Join the datasets and compute bars for plot + the top overall performer

all_mains <- main_models %>%
  left_join(main_models_aggregate, by = c("problem" = "problem")) %>%
  mutate(lower_x = balanced_accuracy - 1 * balanced_accuracy_sd,
         upper_x = balanced_accuracy + 1 * balanced_accuracy_sd,
         lower_y = balanced_accuracy_all - 1 * balanced_accuracy_sd_all,
         upper_y = balanced_accuracy_all + 1 * balanced_accuracy_sd_all) %>%
  mutate(top_performer = ifelse(balanced_accuracy > balanced_accuracy_all, method_set, method))

#------------------ Analysis I: Top performer per problem -----------------

# Create palette for whoever is top performer

mypal <- c("All Features" = "black",
           "catch22" = "#1B9E77",
           "feasts" = "#D95F02",
           "kats" = "#7570B3",
           "tsfeatures" = "#E7298A",
           "tsfel" = "#66A61E",
           "tsfresh" = "#E6AB02")

# Define coordinates for upper triangle to shade

upper_tri <- data.frame(x = c(0, 0, 100), y = c(0, 100, 100))

# Draw scatterplot

p <- all_mains %>%
  ggplot(aes(x = balanced_accuracy, y = balanced_accuracy_all)) +
  geom_polygon(data = upper_tri, aes(x = x, y = y), fill = "steelblue2", alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, colour = "grey50", lty = "dashed") +
  geom_errorbar(aes(ymin = lower_y, ymax = upper_y, colour = top_performer)) +
  geom_errorbarh(aes(xmin = lower_x, xmax = upper_x, colour = top_performer)) +
  geom_point(aes(colour = top_performer), size = 2) +
  annotate("text", x = 75, y = 10, label = "Single feature set better") +
  annotate("text", x = 25, y = 90, label = "All features better") +
  labs(title = "Comparison of top classifier across UCR/UEA repository univariate problems",
       subtitle = "Error bars are +- 1 SD obtained through 10-fold CV",
       x = "Classification accuracy individual set (%)",
       y = "Classification accuracy all features (%)",
       colour = NULL) +
  scale_x_continuous(labels = function(x)paste0(x, "%")) + 
  scale_y_continuous(labels = function(x)paste0(x, "%")) + 
  scale_colour_manual(values = mypal) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

print(p)
ggsave("output/all_versus_sets.pdf", p)
