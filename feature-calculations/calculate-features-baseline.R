#---------------------------------------
# This script uses theft to calculate
# features for all sets on all problems.
# It requires get-uea-ucr-datasets.py to
# have been run first.
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 4 May 2026
#---------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(tsibble)
library(theft)

#' Function to load a UEA/UCR dataset that has been saved from `aeon` in Python and calculate baseline feature sets on it
#' 
#' @param problem \code{character} denoting the dataset to work on
#' @return \code{feature_calculations} object containing the features
#' @author Trent Henderson
#' 

calculate_uea_ucr_baseline <- function(problem){
  
  cat(paste0("Evaluating: ", problem, "\n"))
  
  # Check that there is an X,y for train and test and catch other errors
  
  if((length(list.files(paste0("data/", problem))) == 0) | 
     (paste0(problem, ".Rda") %in% list.files("feature-calculations/baseline-features")) |
     (length(list.files(paste0("data/", problem))) != 4)){
    return(NA)
  } else{
    
    # Pull data from saved format
    
    train_X <- read.csv(paste0("data/", problem, "/", problem, "_train_X.csv")) |> 
      mutate(id = row_number())
    
    train_y <- read.csv(paste0("data/", problem, "/", problem, "_train_y.csv")) |> 
      mutate(id = row_number())
    
    test_X <- read.csv(paste0("data/", problem, "/", problem, "_test_X.csv")) |> 
      mutate(id = row_number() + nrow(train_y))
    
    test_y <- read.csv(paste0("data/", problem, "/", problem, "_test_y.csv")) |> 
      mutate(id = row_number() + nrow(train_y))
    
    # Wrangle into long format and convert to {tsibble} for {theft}
    
    train <- train_X |>
      pivot_longer(cols = !id, names_to = "timepoint", values_to = "values") |>
      mutate(timepoint = as.integer(gsub("X", "\\1", timepoint)) + 1) |>
      inner_join(train_y, by = c("id" = "id")) |>
      mutate(train_test = "Train")
    
    test <- test_X |>
      pivot_longer(cols = !id, names_to = "timepoint", values_to = "values") |>
      mutate(timepoint = as.integer(gsub("X", "\\1", timepoint)) + 1) |>
      inner_join(test_y, by = c("id" = "id")) |>
      mutate(train_test = "Test")
    
    tsbl <- bind_rows(train, test) |>
      mutate(target = as.factor(target)) |>
      mutate(id = as.character(id))
    
    label <- tsbl |>
      dplyr::select(c(id, train_test)) |>
      distinct() |>
      mutate(problem = problem)
    
    tsbl <- tsbl |> 
      dplyr::select(-c(train_test)) |>
      as_tsibble(key = c("id", "target"), index = "timepoint")
    
    #-------------------
    # Calculate features
    #-------------------
    
    # Quantiles
    
    quantiles <- try(
      calculate_features(
        tsbl,
        feature_set = c("quantiles"),
        z_score = TRUE,
        warn = FALSE,
        seed = 123
      )
    )
    
    if(inherits(quantiles, "try-error")){
      return(NA) # Exit if the calculation errors else save the object
    } else{
      
      # Compute our new FFT + quantiles bespoke set
      
      fftquantiles <- calculate_features(
        tsbl,
        feature_set = c("fftquantiles"),
        z_score = TRUE,
        warn = FALSE,
        seed = 123,
        squared = TRUE
      ) |> 
        mutate(feature_set = "FFT (Mag^2 + Angle) + quantiles")
      
      # Pull FFT baseline from tsfresh for consistency
      
      load(paste0("feature-calculations/features/", problem, ".Rda"))
      
      fft <- features |>
        filter(feature_set == "tsfresh") |>
        filter(grepl("fft_coefficient", names)) |>
        mutate(feature_set = "FFT coefficients")
      
      stopifnot(length(unique(fft$names)) == 400)
      
      #-----------------------------------
      # Make MECE variants we want to test
      #-----------------------------------
      
      # 400 FFT coefficients from {tsfresh} + quantiles
      
      union_set <- bind_rows(quantiles, fft) |>
        mutate(feature_set = "FFT (Re, Im, Mag, Angle) + quantiles")
      
      # Pull out individual components
      
      fft_re <- fft |>
        filter(grepl('attr_"real"', names))
      
      fft_imag <- fft |>
        filter(grepl('attr_"imag"', names))
      
      fft_abs <- fft |>
        filter(grepl('attr_"abs"', names))
      
      fft_angle <- fft |>
        filter(grepl('attr_"angle"', names))
      
      stopifnot(length(unique(fft_re$names)) == 100)
      stopifnot(length(unique(fft_imag$names)) == 100)
      stopifnot(length(unique(fft_abs$names)) == 100)
      stopifnot(length(unique(fft_angle$names)) == 100)
      
      # Construct dual sets
      
      fft_quantiles_re_imag <- bind_rows(quantiles, fft_re, fft_imag) |>
        mutate(feature_set = "FFT (Re, Im) + quantiles")
      
      fft_quantiles_abs_angle <- bind_rows(quantiles, fft_abs, fft_angle) |>
        mutate(feature_set = "FFT (Mag, Angle) + quantiles")
      
      fft_abs_log <- fft_abs |>
        mutate(values = log(values))
      
      fft_quantiles_log_abs_angle <- bind_rows(quantiles, fft_abs_log, fft_angle) |>
        mutate(feature_set = "FFT (log(Mag), Angle) + quantiles")
      
      # Bind all as one and save
      
      all_baseline <- bind_rows(quantiles, fftquantiles, fft, union_set, fft_quantiles_re_imag, fft_quantiles_abs_angle, fft_quantiles_log_abs_angle)
      save(all_baseline, file = paste0("feature-calculations/baseline-features/", problem, ".Rda"))
    }
  }
}

#--------------- Run the calculations ---------------

gsub("\\.Rda", "\\1", list.files("feature-calculations/features")) |>
  purrr::map(~calculate_uea_ucr_baseline(problem = .x))
