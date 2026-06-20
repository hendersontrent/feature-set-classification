#--------------------------------------
# This script statistically compares
# XGBoost and linear SVM for each
# feature set
#--------------------------------------

#--------------------------------------
# Author: Trent Henderson, 20 June 2026
#--------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(correctR)
library(ggplot2)

#--------------- Define functions ---------------

#' Get train-test sample sizes for each problem
#'
#' @return \code{data.frame} of problem summaries
#' @author Trent Henderson
#'

get_n <- function(){
  
  n_storage <- vector(mode = "list", length = length(list.files("feature-calculations/train-test-labels/")))
  
  for(i in 1:length(n_storage)){
    rm(label)
    load(paste0("feature-calculations/train-test-labels/", list.files("feature-calculations/train-test-labels/")[i]))
    
    n_tmp <- label |>
      distinct() |>
      reframe(counter = n(), .by = c("problem", "train_test")) |>
      pivot_wider(id_cols = "problem", names_from = "train_test", values_from = "counter")
    
    n_storage[[i]] <- n_tmp
  }
  
  n_storage <- do.call("rbind", n_storage)
  return(n_storage)
}

#' Pull and merge linear SVM and XGBoost classification results
#'
#' @return \code{data.frame} containing per-resample results for both models
#' @author Trent Henderson
#'

linear_xg <- function(){
  
  # Pull linear SVM results
  
  files <- list.files(paste0("classification-models/results/svm"))
  accuracies <- vector(mode = "list", length = length(files))
  
  for(i in files){
    feature_sets <- read.csv(paste0("classification-models/results/svm/", i))
    accuracies[[match(i, files)]] <- feature_sets
  }
  
  accuracies <- do.call("rbind", accuracies) |>
    filter(feature_set != "timegp") |> # From another related project which used the same methodology
    rename(accuracy_linear = accuracy) |>
    dplyr::select(-c(classifier))
  
  # Pull XGBoost results
  
  files2 <- list.files(paste0("classification-models/results/xgboost"))
  accuracies2 <- vector(mode = "list", length = length(files2))
  
  for(j in files2){
    feature_sets2 <- read.csv(paste0("classification-models/results/xgboost/", j))
    accuracies2[[match(j, files2)]] <- feature_sets2
  }
  
  accuracies2 <- do.call("rbind", accuracies2) |>
    rename(accuracy_xg = accuracy) |>
    dplyr::select(-c(classifier))
  
  # Merge
  
  accuracies3 <- accuracies |>
    inner_join(accuracies2) |>
    filter(feature_set %in% c("catch22", "feasts", "tsfeatures", "Kats", "TSFEL",
                              "tsfresh", "FFT (Mag^2 + Angle) + quantiles")) |>
    filter(problem != "Fungi")
  
  return(accuracies3)
}

#' Compute a resampled corrected t-test contrasting XGBoost against the linear SVM for a single feature set and problem combination
#'
#' @param data \code{data.frame} of merged classification results
#' @param theproblem \code{character} denoting the dataset to work on
#' @param thefeatureset \code{character} denoting the feature set to work on
#' @param problem_data \code{data.frame} containing problem summary information
#' @return \code{data.frame}
#' @author Trent Henderson
#'

contrast_models <- function(data, theproblem, thefeatureset, problem_data){
  
  tmp <- data |>
    filter(problem == theproblem, feature_set == thefeatureset) |>
    arrange(resample)
  
  # Skip combinations with missing accuracies
  
  if(nrow(tmp) == 0 | any(is.na(tmp$accuracy_xg)) | any(is.na(tmp$accuracy_linear))){
    return(NULL)
  }
  
  # Filter to get parameters for corrected t-test
  
  params <- problem_data |>
    filter(problem == theproblem)
  
  # Set up vectors
  
  x <- tmp$accuracy_xg
  y <- tmp$accuracy_linear
  
  # Compute test
  
  t_test <- correctR::resampled_ttest(x = x, y = y, n = 30,
                                      n1 = as.integer(params$Train), n2 = as.integer(params$Test),
                                      tailed = "one", greater = "x")
  
  out <- data.frame(feature_set = thefeatureset,
                    problem = theproblem,
                    statistic = as.numeric(t_test$statistic),
                    p.value = as.numeric(t_test$p.value),
                    mean_xg = mean(x, na.rm = TRUE),
                    mean_linear = mean(y, na.rm = TRUE))
  
  return(out)
}

#--------------- Run the function ---------------

# Get problem summaries (train/test sizes)

problem_summaries <- get_n()

# Pull and merge results

results <- linear_xg()

# Compute comparisons

comparisons <- results |>
  distinct(feature_set, problem) |>
  purrr::pmap_df(function(feature_set, problem){
    contrast_models(data = results, theproblem = problem,
                    thefeatureset = feature_set, problem_data = problem_summaries)
    }
  )

#--------------- Draw plot ---------------

# Compute overall results

comparisons |> 
  mutate(significant = ifelse(p.value < .05, TRUE, FALSE)) |> 
  mutate(.category = case_when(
          significant & mean_xg > mean_linear ~ "XGBoost better",
          significant & mean_xg < mean_linear ~ "Linear SVM better",
          !significant                        ~ "Tie")) |>
  reframe(.count = n(), .by = ".category") |> 
  mutate(pct = .count / sum(.count) * 100)

# Draw plot

p <- comparisons |> 
  mutate(significant = ifelse(p.value < .05, TRUE, FALSE)) |> 
  mutate(.category = case_when(
    significant & mean_xg > mean_linear ~ "XGBoost statistically better",
    significant & mean_xg < mean_linear ~ "Linear SVM statistically better",
    !significant                        ~ "Tie")) |>
  reframe(.count = n(), .by = c(".category", "feature_set")) |> 
  mutate(.category = factor(.category, levels = c("XGBoost statistically better", "Tie"))) |>
  group_by(feature_set) |>
  mutate(pct = .count / sum(.count) * 100) |>
  ungroup() |>
  mutate(feature_set = ifelse(feature_set == "FFT (Mag^2 + Angle) + quantiles", "FFT (Mag² + Angle) +\nquantiles", feature_set)) |>
  mutate(feature_set = factor(feature_set, levels = c("catch22", "feasts", "Kats",
                                                      "tsfeatures", "TSFEL", "tsfresh",
                                                      "FFT (Mag² + Angle) +\nquantiles"))) |>
  ggplot(aes(x = feature_set, y = .count, fill = .category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, digits = 2), "%")), size = 5, 
            position = position_stack(vjust = 0.5), colour = "white", fontface = "bold") +
  labs(x = "Feature set",
       y = "Number of problems",
       fill = NULL) +
  scale_fill_manual(values = c("Tie" = pals[1], "XGBoost statistically better" = pals[2])) +
  theme_bw() +
  theme(legend.position = "bottom")

print(p)
ggsave("output/xgboost-vs-svm-stat-test.pdf", p, width = 8, height = 8)
