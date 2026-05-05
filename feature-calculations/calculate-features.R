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

#--------------- Define functions ---------------

#' Download and install tsfresh, TSFEL, and Kats from Python into a new virtual environment. This is a modified version of {theft::install_python_pkgs}
#'
#' @importFrom reticulate virtualenv_create virtualenv_install
#' 
#' @param venv \code{character} specifying the name of the new virtual environment to create
#' @param python \code{character} specifying the filepath to the Python interpreter to use. Python 3.10 is recommended
#' @param hctsa \code{Boolean} denoting whether to install pyhctsa or not. Defaults to \code{FALSE}
#' @return no return value; called for side effects
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' install_python_pkgs2("theft-test", "/usr/local/bin/python3.10")
#' }
#' 
install_python_pkgs2 <- function(venv, python, hctsa = FALSE){
  reticulate::virtualenv_create(venv, python)
  reticulate::virtualenv_install(venv, "tsfel")
  reticulate::virtualenv_install(venv, "tsfresh")
  reticulate::virtualenv_install(venv, "git+https://github.com/hendersontrent/theft-kats.git")
  
  if(hctsa){
    reticulate::virtualenv_install(venv, "git+https://github.com/DynamicsAndNeuralSystems/pyhctsa.git")
  }
  
  reticulate::virtualenv_install(venv, "scipy==1.14.0")
  reticulate::virtualenv_install(venv, "wheel==0.45.1")
  reticulate::virtualenv_install(venv, "packaging==21.3")
  reticulate::virtualenv_install(venv, "numpy==1.26")
}

#' Function to load a UEA/UCR dataset that has been saved from `aeon` in Python and calculate features on it
#' 
#' @param problem \code{character} denoting the dataset to work on
#' @return \code{feature_calculations} object containing the features
#' @author Trent Henderson
#' 

calculate_uea_ucr_features <- function(problem){
  
  cat(paste0("Evaluating: ", problem, "\n"))
  
  # Check that there is an X,y for train and test and catch other errors
  
  if((length(list.files(paste0("data/", problem))) == 0) | 
     (paste0(problem, ".Rda") %in% list.files("feature-calculations/features")) |
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
       
       # Calculate features and save
       
       features <- try(
         calculate_features(
           tsbl,
           feature_set = c("catch22", "feasts", "tsfeatures", "kats", "tsfel", "tsfresh"),
           z_score = TRUE,
           n_jobs = 8,
           warn = FALSE,
           seed = 123,
           use_compengine = TRUE
         )
       )
       
       if(inherits(features, "try-error")){
         return(NA) # Exit if the calculation errors else save the object
       } else{
         save(features, file = paste0("feature-calculations/features/", problem, ".Rda"))
         save(label, file = paste0("feature-calculations/train-test-labels/", problem, ".Rda"))
       }
  }
}

#--------------- Set up Python environment for theft ---------------

#install_python_pkgs2(venv = "uea-ucr-2026", python = "/Users/trenthenderson/.pyenv/versions/3.10.20/bin/python3.10")
init_theft("uea-ucr-2026")

#--------------- Run the calculations ---------------

list.files("data") |>
  purrr::map(~calculate_uea_ucr_features(problem = .x))
