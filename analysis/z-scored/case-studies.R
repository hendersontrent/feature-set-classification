#----------------------------------------
# This script sets out to analyse some
# case study problems to more deeply
# understand why a given feature set
# performs well or poorly on it
#----------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 19 September 2022
#-------------------------------------------

# Load classification results

load("data/outputs_z.Rda")

outputs_z <- outputs_z %>%
  mutate(method = case_when(
    method == "tsfel" ~ "TSFEL",
    method == "kats"  ~ "Kats",
    TRUE              ~ method)) %>%
  filter(problem %in% c("Plane", "PhalangesOutlinesCorrect", "FreezerSmallTrain")) # Case study problems of interest

# Load raw time series

load("data/TimeSeriesData.Rda")

plane <- TimeSeriesData %>%
  filter(problem == "Plane")

phalanges <- TimeSeriesData %>%
  filter(problem == "PhalangesOutlinesCorrect")

freezer <- TimeSeriesData %>%
  filter(problem == "FreezerSmallTrain")

rm(TimeSeriesData)

# Load feature calculations

load("data/feature-calcs/z-scored/Plane.Rda")
plane_feats <- outs_z
load("data/feature-calcs/z-scored/PhalangesOutlinesCorrect.Rda")
phalanges_feats <- outs_z
load("data/feature-calcs/z-scored/FreezerSmallTrain.Rda")
freezer_feats <- outs_z
rm(outs_z)

# Calculate overall mean performance for each problem and set

perform <- outputs_z %>%
  group_by(problem, method) %>%
  summarise(x = mean(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup() 

#------------------ Define useful functions ----------------

#' Sample IDs by class
#' @param data the dataframe containing time-series data
#' @param group_name string specifying the class to filter by
#' @param n number of samples to generate
#' @return object of class vector
#' @author Trent Henderson
#' 

draw_samples <- function(data, group_name, n){
  
  samps <- data %>%
    filter(target == group_name) %>%
    dplyr::select(id) %>%
    distinct() %>%
    pull(id) %>%
    sample(size = n)
  
  return(samps)
}

#' Draw time series plots by class
#' @param data the dataframe containing time series data
#' @param n number of samples to take
#' @param seed fix R's pseudo-random number generator
#' @author Trent Henderson
#'

plot_samples <- function(data, n = 2, seed = 123){
  
  set.seed(seed)
  
  # Generate IDs to sample
  
  ids <- unique(data$target) %>%
    purrr::map(~ draw_samples(data = data, group_name = .x, n = n)) %>%
    unlist()
  
  # Draw plot
  
  p <- data %>%
    filter(id %in% ids) %>%
    mutate(id = factor(id, levels = ids)) %>%
    ggplot(aes(x = timepoint, y = values, colour = target)) +
    geom_line() +
    labs(title = paste0("Random sample of ", n, " time series from each class for ", unique(data$problem)),
         x = "Time",
         y = "Value",
         colour = "Class") +
    theme_bw() +
    theme(legend.position = "bottom",
          strip.background = element_blank(),
          strip.text = element_text(face = "bold")) +
    facet_wrap(~id, ncol = n, nrow = length(unique(data$target)))
  
  return(p)
}

#' Draw all time series faceted by class with a mean line
#' @param data the dataframe containing time series data
#' @author Trent Henderson
#'

plot_all_ts <- function(data){
  
  # Calculate mean
  
  mu <- data %>%
    mutate(target = as.factor(target)) %>%
    group_by(timepoint, target) %>%
    summarise(mu = mean(values, na.rm = TRUE)) %>%
    ungroup()
  
  # Draw plot
  
  p <- data %>%
    mutate(id = as.factor(id),
           target = as.factor(target))
  
  p <- p %>%
    ggplot(aes(x = timepoint, y = values)) +
    geom_line(aes(group = id, colour = set_split), size = 0.6, alpha = 0.8) +
    geom_line(data = mu, aes(x = timepoint, y = mu), size = 1, colour = "#b41e51") +
    labs(title = paste0("Time series plots for each class for ", unique(data$problem)),
         subtitle = "Mean is represented by thick red line.",
         x = "Time",
         y = "Value",
         colour = NULL) +
    theme_bw() +
    theme(legend.position = "bottom",
          strip.background = element_blank(),
          strip.text = element_text(face = "bold")) +
    facet_wrap(~target, ncol = 1, nrow = length(unique(p$target)))
  
  return(p)
}

#------------------ Case study I: Plane ----------------

# Draw plot

plot_samples(data = plane, n = 2, seed = 123)

# Identify top features

plane_top <- compute_top_features2(plane_feats, 
                                   id_var = "id", 
                                   group_var = "group",
                                   num_features = 40, 
                                   method = "z-score",
                                   test_method = "svmLinear",
                                   use_balanced_accuracy = TRUE,
                                   use_k_fold = TRUE,
                                   num_folds = 10,
                                   use_empirical_null =  TRUE,
                                   null_testing_method = "ModelFreeShuffles",
                                   p_value_method = "gaussian",
                                   num_permutations = 1000,
                                   seed = 123)

save(plane_top, file = "data/plane_top.Rda")

# Draw plots like in the catch22 paper

plane_plot <- plot_all_ts(data = plane)
print(plane_plot)
ggsave("output/plane_sample.pdf", plot = plane_plot, units = "in", height = 12, width = 8)

#------------------ Case study II: PhalangesOutlinesCorrect ------------------

# Draw plot

plot_samples(data = phalanges, n = 3, seed = 123)

# Identify top features

phalanges_top <- compute_top_features2(phalanges_feats, 
                                       id_var = "id", 
                                       group_var = "group",
                                       num_features = 40, 
                                       method = "z-score",
                                       test_method = "svmLinear",
                                       use_balanced_accuracy = TRUE,
                                       use_k_fold = TRUE,
                                       num_folds = 10,
                                       use_empirical_null =  TRUE,
                                       null_testing_method = "ModelFreeShuffles",
                                       p_value_method = "gaussian",
                                       num_permutations = 1000,
                                       seed = 123)

save(phalanges_top, file = "data/phalanges_top.Rda")

# Draw plots like in the catch22 paper

phalanges_plot <- plot_all_ts(data = phalanges)
print(phalanges_plot)
ggsave("output/phalanges_sample.pdf", plot = phalanges_plot)

#------------------ Case study III: FreezerSmallTrain -----------------

# Draw plot

plot_samples(data = freezer, n = 2, seed = 123)

# Identify top features

freezer_top <- compute_top_features2(freezer_feats, 
                                     id_var = "id", 
                                     group_var = "group",
                                     num_features = 40, 
                                     method = "z-score",
                                     test_method = "svmLinear",
                                     use_balanced_accuracy = TRUE,
                                     use_k_fold = TRUE,
                                     num_folds = 10,
                                     use_empirical_null =  TRUE,
                                     null_testing_method = "ModelFreeShuffles",
                                     p_value_method = "gaussian",
                                     num_permutations = 1000,
                                     seed = 123)

save(freezer_top, file = "data/freezer_top.Rda")

# Draw plots like in the catch22 paper

freezer_plot <- plot_all_ts(data = freezer)
print(freezer_plot)
ggsave("output/freezer_sample.pdf", plot = freezer_plot, units = "in", height = 12, width = 8)
