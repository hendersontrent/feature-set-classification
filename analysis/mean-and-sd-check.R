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

# Get chance probabilities

num_classes <- TimeSeriesData %>%
  dplyr::select(c(target, problem)) %>%
  distinct() %>%
  group_by(problem) %>%
  summarise(classes = n()) %>%
  ungroup() %>%
  mutate(chance = 1 / classes)

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
#' @param theproblem string of the problem to calculate for
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
                                             num_resamples = 30,
                                             problem_name = theproblem,
                                             conf_mat = FALSE) %>%
    mutate(problem = theproblem)
  
  return(results)
}

calculate_accuracy_for_mean_sd_safe <- purrr::possibly(calculate_accuracy_for_mean_sd, otherwise = NULL)

mean_sd_outputs <- unique(mean_sd_test$problem) %>%
  purrr::map_df(~ calculate_accuracy_for_mean_sd_safe(data = mean_sd_test, theproblem = .x))

save(mean_sd_outputs, file = "data/mean_sd_outputs.Rda")

#------------- Final list of problems --------------

# Find out for which problems mean and SD significantly outperformed chance

benchmark_keepers <- mean_sd_outputs %>%
  group_by(problem) %>%
  summarise(mu = mean(accuracy, na.rm = TRUE),
            sigma = sd(accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(num_classes, by = c("problem" = "problem")) %>%
  mutate(p.value = pnorm(chance, 
                         mean = mu,
                         sd = sigma,
                         lower.tail = FALSE),
         p.value = 1 - p.value) %>%
  mutate(category = ifelse(p.value <= 0.05, "Significant", "Non-significant")) %>%
  dplyr::select(problem, p.value, category)

save(benchmark_keepers, file = "data/benchmark_keepers.Rda")

#------------- Results visualisation --------------

mypal2 <- c("<= chance" = mypal[2],
            "> chance" = mypal[1])

p <- mean_sd_outputs %>%
  mutate(accuracy = accuracy * 100) %>%
  group_by(problem) %>%
  summarise(mu = mean(accuracy, na.rm = TRUE),
            lower = mean(accuracy, na.rm = TRUE) - 1.96 * sd(accuracy, na.rm = TRUE),
            upper = mean(accuracy, na.rm = TRUE) + 1.96 * sd(accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(num_classes, by = c("problem" = "problem")) %>%
  left_join(benchmark_keepers, by = c("problem" = "problem")) %>%
  mutate(chance = chance * 100) %>%
  mutate(performance = ifelse(category == "Significant", "> chance", "<= chance")) %>%
  ggplot() +
  geom_errorbar(aes(ymin = lower, ymax = upper, x = reorder(problem, mu), y = mu, colour = performance)) +
  geom_point(aes(x = reorder(problem, mu), y = mu, colour = performance)) +
  geom_point(aes(x = reorder(problem, mu), y = chance), colour = "black", shape = 3, size = 1) +
  labs(x = "Problem",
       y = "Classification accuracy (%)",
       colour = NULL) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(from = 0, to = 100, by = 20),
                     labels = function(x)paste0(x, "%")) + 
  scale_colour_manual(values = mypal2) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 11))

print(p)
ggsave("output/mean-and-sd-resamples.pdf", plot = p, units = "in", height = 16, width = 11)
