#------------------------------------------
# This script sets out to compute features
# for each set and time-series problem
#
# NOTE: This script requires setup.R and
# analysis/prepare-time-series-data.R 
# to have been run first
#------------------------------------------

#---------------------------------------
# Author: Trent Henderson, 13 April 2023
#---------------------------------------

# Load data

load("data/TimeSeriesData.Rda")
load("data/good_keepers.Rda")

#------------- Calculate features --------------

# Calculate quantiles

quantiles <- TimeSeriesData %>%
  filter(problem %in% good_keepers) %>%
  group_by(problem, id, target, set_split) %>%
  arrange(timepoint) %>%
  mutate(values = (values - mean(values, na.rm = TRUE)) / sd(values, na.rm = TRUE)) %>% # z-score
  summarise(basicproperties::get_properties(values)) %>%
  ungroup() %>%
  filter(feature_set == "quantiles") %>% # Filter to just quantiles since we will use {tsfresh}'s FFT values for consistency
  rename(group = target,
         names = feature_name,
         method = feature_set) # Alignment with prior version of {theft}

# Get FFT coefficients already calculated

data_files <- list.files("data/feature-calcs/z-scored", full.names = TRUE, pattern = "\\.Rda")
fft_values <- vector(mode = "list", length = length(data_files))

for(d in data_files){
  load(d)
  fft_values[[match(d, data_files)]] <- outs
}

fft_values <- do.call("rbind", fft_values) %>%
  filter(grepl("fft_coefficient", names)) %>%
  mutate(method = "fft")

length(unique(fft_values$names))
rm(outs, d, data_files)

train_test_ids <- TimeSeriesData %>%
  dplyr::select(c(problem, id, set_split)) %>%
  distinct()

fft_values <- fft_values %>%
  mutate(problem = gsub(".*_", "\\1", id)) %>%
  left_join(train_test_ids, by = c("id" = "id", "problem" = "problem"))

# Bind

fft_quantiles <- bind_rows(quantiles, fft_values)

# Save for future use

rm(TimeSeriesData, quantiles, fft_values)
save(fft_quantiles, file = "data/feature-calcs/basic-properties/fft_quantiles.Rda")

#------------- Fit classifiers --------------

#' Fit classifiers for all feature sets
#' 
#' @param data \code{data.frame} of feature information
#' @param problem_name \code{string} denoting the problem to analyse
#' @param n_resamples \code{integer} denoting the number of resamples to calculate. Defaults to \code{30}
#' @return \code{data.frame} of classification results
#' @author Trent Henderson
#' 

fit_bp_classifiers <- function(data, problem_name, n_resamples = 30){
  
  outs <- data %>%
    filter(problem == problem_name)
  
  outputs <- unique(outs$method) %>%
      purrr::map_dfr(~ evaluate_performance(outs, problem_name = problem_name, 
                                            n_resamples = n_resamples, feature_set = .x))
  
  return(outputs)
}

# Run function

outputs_z_bp <- good_keepers %>%
  purrr::map_dfr(~ fit_bp_classifiers(data = fft_quantiles, problem_name = .x, n_resamples = 30))

save(outputs_z_bp, file = "data/outputs_z_bp.Rda")
