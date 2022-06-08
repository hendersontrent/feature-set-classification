#------------------------------------------
# This script sets out to compute features
# for mean and SD to understand which
# problems are appropriately z-scored to
# retain
#
# NOTE: This script requires setup.R and
# analysis/prepare-time-series-data.R 
# to have been run first
#-----------------------------------------

#-------------------------------------
# Author: Trent Henderson, 7 June 2022
#-------------------------------------

# Load data

load("data/TimeSeriesData.Rda")

#------------- Feature extraction --------------

#' Function to map over datasets to avoid massive dataframe processing times / crashes
#' @param data the dataset containing all raw time series
#' @param theproblem string specifying the problem to calculate features for
#' @returns an object of class dataframe
#' @author Trent Henderson
#' 

extract_mean_and_sd <- function(data, theproblem){
  
  message(paste0("Doing problem ", match(theproblem, unique(data$problem)), "/", length(unique(data$problem))))
  
  # Filter to problem of interest and calculate features
  
  outs <- data %>%
    filter(problem == theproblem) %>%
    dplyr::rename(group = target) %>%
    dplyr::select(c(.data$id, .data$timepoint, .data$values, .data$group, .data$set_split, .data$problem)) %>%
    dplyr::group_by(.data$id, .data$group, .data$set_split, .data$problem) %>%
    dplyr::arrange(.data$timepoint) %>%
    dplyr::summarise(mu = mean(.data$values, na.rm = TRUE),
                     sigma = stats::sd(.data$values, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(method = "Mean and variance") %>%
    tidyr::pivot_longer(cols = mu:sigma, names_to = "names", values_to = "values")
  
  return(outs)
}

# Run the function

extract_mean_and_sd_safe <- purrr::possibly(extract_mean_and_sd, otherwise = NULL)

mean_sd_test <- unique(TimeSeriesData$problem) %>%
  purrr::map_df(~ extract_mean_and_sd_safe(data = TimeSeriesData, theproblem = .x))

rm(TimeSeriesData)

#------------- Classification performance --------------

# NOTE: Goal is to determine is mean and variance can outperform chance to determine effect of z-scoring

#-----------------
# Resamples method
#-----------------

#' Function to map classification performance calculations over datasets/problems
#' @param data the dataframe to operate on
#' @param theproblem filepath to the feature data
#' @returns an object of class dataframe
#' @author Trent Henderson
#' 

calculate_accuracy_for_mean_sd <- function(data, theproblem){
  
  message(paste0("Doing problem ", match(theproblem, unique(data$problem)), "/", length(unique(data$problem))))
          
  outs <- data %>%
    filter(problem == theproblem)
  
  # Fit multi-feature classifiers by feature set
  
  results <- fit_multi_feature_classifier_tt(outs, 
                                             id_var = "id", 
                                             group_var = "group",
                                             by_set = FALSE, 
                                             test_method = "svmLinear", 
                                             use_balanced_accuracy = TRUE,
                                             use_k_fold = TRUE, 
                                             num_folds = 10, 
                                             num_resamples = 10) %>%
    mutate(problem = theproblem)
  
  return(results)
}

calculate_accuracy_for_mean_sd_safe <- purrr::possibly(calculate_accuracy_for_mean_sd, otherwise = NULL)

mean_sd_outputs <- unique(mean_sd_test$problem) %>%
  purrr::map_df(~ calculate_accuracy_for_mean_sd_safe(data = mean_sd_test, theproblem = .x))

save(mean_sd_outputs, file = "data/mean_sd_outputs.Rda")

#---------------------------
# Model-free shuffles method
#---------------------------

#' Function to map classification performance calculations over datasets/problems
#' @param data the dataframe to operate on
#' @param theproblem filepath to the feature data
#' @returns an object of class dataframe
#' @author Trent Henderson
#' 

calculate_accuracy_model_free <- function(data, theproblem){
  
  message(paste0("Doing problem ", match(theproblem, unique(data$problem)), "/", length(unique(data$problem))))
  
  outs <- data %>%
    filter(problem == theproblem)
  
  # Fit multi-feature classifiers by feature set
  
  results <- fit_multi_feature_classifier(outs, 
                                          id_var = "id", 
                                          group_var = "group", 
                                          by_set = FALSE, 
                                          test_method = "svmLinear",
                                          use_balanced_accuracy = TRUE,
                                          use_k_fold = TRUE,
                                          num_folds = 10,
                                          use_empirical_null =  TRUE,
                                          null_testing_method = "model free shuffles",
                                          p_value_method = "gaussian",
                                          num_permutations = 10000,
                                          seed = 123)$TestStatistics %>%
    mutate(problem = theproblem)
  
  return(results)
}

calculate_accuracy_model_free_safe <- purrr::possibly(calculate_accuracy_model_free, otherwise = NULL)

mean_sd_outputs_model_free <- unique(mean_sd_test$problem) %>%
  purrr::map_df(~ calculate_accuracy_model_free_safe(data = mean_sd_test, theproblem = .x))

save(mean_sd_outputs_model_free, file = "data/mean_sd_outputs_model_free.Rda")

#----------------------
# Results visualisation
#----------------------

# Get chance probabilities

load("data/TimeSeriesData.Rda")

num_classes <- TimeSeriesData %>%
  dplyr::select(c(target, problem)) %>%
  distinct() %>%
  group_by(problem) %>%
  summarise(classes = n()) %>%
  ungroup() %>%
  mutate(chance = 1 / classes)

rm(TimeSeriesData)

# Analysis I : Resamples version

mypal <- c("< chance" = "#7570B3",
           ">= chance" = "#1B9E77")

p <- mean_sd_outputs %>%
  dplyr::select(-c(category, classifier_name, statistic_name)) %>%
  pivot_longer(cols = accuracy:balanced_accuracy, names_to = "metric", values_to = "values") %>%
  group_by(problem, metric) %>%
  summarise(mu = mean(values, na.rm = TRUE),
            lower = quantile(values, prob = 0.025),
            upper = quantile(values, prob = 0.975)) %>%
  ungroup() %>%
  mutate(metric = ifelse(metric == "accuracy", "Accuracy", "Balanced accuracy")) %>%
  left_join(num_classes, by = c("problem" = "problem")) %>%
  mutate(performance = ifelse(mu >= chance, ">= chance", "< chance")) %>%
  ggplot() +
  geom_errorbar(aes(ymin = lower, ymax = upper, x = reorder(problem, mu), y = mu, colour = performance)) +
  geom_point(aes(x = reorder(problem, mu), y = mu, colour = performance)) +
  geom_point(aes(x = reorder(problem, mu), y = chance), colour = "black", shape = 3, size = 1) +
  labs(title = "Classification performance of mean and SD using 30 resamples",
       subtitle = "Points indicate mean accuracy, bars indicate 95% quantile. Black crosses indicate chance",
       x = "Problem",
       y = "Accuracy",
       colour = NULL) +
  scale_colour_manual(values = mypal) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.y = element_text(size = 5)) +
  facet_wrap(~metric)

print(p)
ggsave("output/mean-and-sd-resamples.pdf", plot = p)

# Analysis II: Model free version

mypal2 <- c("Not significant" = "#7570B3",
            "Significant" = "#1B9E77")

accuracies <- mean_sd_outputs_model_free %>%
  dplyr::select(-c(classifier_name, statistic_name, p_value_accuracy, p_value_balanced_accuracy)) %>%
  pivot_longer(cols = accuracy:balanced_accuracy, names_to = "metric", values_to = "values") %>%
  mutate(metric = ifelse(metric == "accuracy", "Accuracy", "Balanced accuracy"))

significance_stats <- mean_sd_outputs_model_free %>%
  dplyr::select(-c(classifier_name, statistic_name, accuracy, balanced_accuracy)) %>%
  pivot_longer(cols = p_value_accuracy:p_value_balanced_accuracy, names_to = "metric", values_to = "values_p") %>%
  mutate(metric = ifelse(metric == "p_value_accuracy", "Accuracy", "Balanced accuracy")) %>%
  inner_join(accuracies, by = c("problem" = "problem", "metric" = "metric")) %>%
  mutate(significance = ifelse(values_p <= (0.05 / (nrow(accuracies) / 2)), "Significant", "Not significant")) %>%
  mutate(significance = factor(significance, levels = c("Not significant", "Significant")))
  
p1 <- significance_stats %>%
  left_join(num_classes, by = c("problem" = "problem")) %>%
  ggplot() +
  geom_point(aes(x = reorder(problem, values), y = values, colour = significance)) +
  geom_point(aes(x = reorder(problem, values), y = chance), colour = "black", shape = 3, size = 1) +
  labs(title = "Classification performance of mean and SD against 1000 model-free shuffles",
       subtitle = "Bonferroni correction applied with original alpha = 0.05. Black crosses indicate chance",
       x = "Problem",
       y = "Accuracy",
       colour = "Statistical significance") +
  scale_colour_manual(values = mypal2) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.y = element_text(size = 5)) +
  facet_wrap(~metric)

print(p1)
ggsave("output/mean-and-sd-model-free.pdf", plot = p1)
