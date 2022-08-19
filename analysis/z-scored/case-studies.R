#----------------------------------------
# This script sets out to analyse some
# case study problems to more deeply
# understand why a given feature set
# performs well or poorly on it
#----------------------------------------

#----------------------------------------
# Author: Trent Henderson, 18 August 2022
#----------------------------------------

# Load classification results

load("data/outputs_z.Rda")

outputs_z <- outputs_z %>%
  mutate(method = case_when(
    method == "tsfel" ~ "TSFEL",
    method == "kats"  ~ "Kats",
    TRUE              ~ method)) %>%
  filter(problem %in% c("Coffee", "ProximalPhalanxOutlineAgeGroup", "Plane")) # Case study problems of interest

# Load raw time series

load("data/TimeSeriesData.Rda")

coffee <- TimeSeriesData %>%
  filter(problem == "Coffee")

proximal <- TimeSeriesData %>%
  filter(problem == "ProximalPhalanxOutlineAgeGroup")

plane <- TimeSeriesData %>%
  filter(problem == "Plane")

rm(TimeSeriesData)

# Load feature calculations

load("data/feature-calcs/z-scored/Coffee.Rda")
coffee_feats <- outs_z
load("data/feature-calcs/z-scored/ProximalPhalanxOutlineAgeGroup.Rda")
proximal_feats <- outs_z
load("data/feature-calcs/z-scored/Plane.Rda")
plane_feats <- outs_z
rm(outs_z)

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
    facet_wrap(~id)
  
  return(p)
}

#------------------ Case study I: Coffee ------------------

#----------------------------------------
# PREMISE: All sets perform about average
# and we want to understand why
#----------------------------------------

# Draw plot

plot_samples(data = coffee, n = 2, seed = 123)

# Identify top features

coffee_top <- compute_top_features(coffee_feats, 
                                   id_var = "id", 
                                   group_var = "group",
                                   num_features = 20, 
                                   method = "z-score",
                                   test_method = "svmLinear",
                                   use_balanced_accuracy = TRUE,
                                   use_k_fold = TRUE,
                                   num_folds = 10,
                                   use_empirical_null =  TRUE,
                                   null_testing_method = "ModelFreeShuffles",
                                   p_value_method = "gaussian",
                                   num_permutations = 100,
                                   seed = 123)

#------------------ Case study II: ProximalPhalanxOutlineAgeGroup -----------------

#--------------------------------------------
# PREMISE: tsfresh performs far below average
# and we want to understand why
#--------------------------------------------

# Draw plot

plot_samples(data = proximal, n = 2, seed = 123)

# Identify top features

proximal_top <- compute_top_features(proximal_feats, 
                                     id_var = "id", 
                                     group_var = "group",
                                     num_features = 20, 
                                     method = "z-score",
                                     test_method = "svmLinear",
                                     use_balanced_accuracy = TRUE,
                                     use_k_fold = TRUE,
                                     num_folds = 10,
                                     use_empirical_null =  TRUE,
                                     null_testing_method = "ModelFreeShuffles",
                                     p_value_method = "gaussian",
                                     num_permutations = 100,
                                     seed = 123)

#------------------ Case study III: Plane ----------------

#--------------------------------------------
# PREMISE: catch22 performs far above average
# and we want to understand why
#--------------------------------------------

# Draw plot

plot_samples(data = plane, n = 2, seed = 123)

# Identify top features

plane_top <- compute_top_features(plane_feats, 
                                  id_var = "id", 
                                  group_var = "group",
                                  num_features = 20, 
                                  method = "z-score",
                                  test_method = "svmLinear",
                                  use_balanced_accuracy = TRUE,
                                  use_k_fold = TRUE,
                                  num_folds = 10,
                                  use_empirical_null =  TRUE,
                                  null_testing_method = "ModelFreeShuffles",
                                  p_value_method = "gaussian",
                                  num_permutations = 100,
                                  seed = 123)
