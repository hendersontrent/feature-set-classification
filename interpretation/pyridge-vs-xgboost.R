#---------------------------------------
# This script calculates the relationship
# between pyridge and XGBoost NPS values
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 2 July 2026
#---------------------------------------

library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(sandwich)
library(lmtest)
library(marginaleffects)

#--------------- Define functions ---------------

#' Compute the normalised performance score
#' 
#' @param model_type \code{character} denoting the type of model that fit and whose results should be loaded. Can be one of \code{"pyridge"} or \code{"xgboost-default"}
#' @return \code{ggplot} containing the NPS plot
#' @author Trent Henderson
#' 

nps <- function(model_type = c("pyridge", "xgboost-default")){
  
  # Read in data
  
  files <- list.files(paste0("classification-models/results/", model_type, "/"))
  accuracies <- vector(mode = "list", length = length(files))
  
  for(i in files){
    feature_sets <- read.csv(paste0("classification-models/results/", model_type, "/", i))
    accuracies[[match(i, files)]] <- feature_sets
  }
  
  accuracies <- do.call("rbind", accuracies) |>
    filter(feature_set != "timegp") |> # From another related project which used the same methodology
    filter(problem != "Fungi")
  
  #---------------------
  # Compute global stats
  #---------------------
  
  # Calculate using only feature sets
  
  benchmarks <- accuracies |>
    filter(feature_set %in% c("catch22", "feasts", "tsfeatures", "Kats", "TSFEL", "tsfresh")) |>
    reframe(.mean = mean(accuracy, na.rm = TRUE),
            .sd = sd(accuracy, na.rm = TRUE),
            .by = "problem")
  
  #---------------------
  # Calculate NPS values
  #---------------------
  
  # Calculate z-scores
  
  z_scores <- accuracies |>
    reframe(x = mean(accuracy, na.rm = TRUE), .by = c("problem", "feature_set")) |>
    inner_join(benchmarks, by = c("problem" = "problem")) |>
    group_by(problem, feature_set) |>
    mutate(z = (x - .mean) / .sd) |>
    ungroup() |>
    dplyr::select(c(problem, feature_set, z))
  
  return(z_scores)
}

nps_pyridge <- nps("pyridge") |> rename(z_pyridge = z)
nps_xgboost <- nps("xgboost-default") |> rename(z_xgboost = z)

#--------------- Analyse results ---------------

# Join data

joined <- nps_pyridge |>
  inner_join(nps_xgboost, by = c("problem" = "problem", "feature_set" = "feature_set")) |>
  filter(feature_set %in% c("catch22", "feasts", "tsfeatures", "Kats", "TSFEL", "tsfresh"))

#----------------------
# Fit statistical model
#----------------------

# Fit model

mod <- lm(z_xgboost ~ z_pyridge, data = joined)
r2 <- round(summary(mod)$r.squared, digits = 3)

# Compute robust effect and predictions

coefs <- coeftest(mod, vcov = vcovHC(mod, type = "HC3"))
coefs_ci <- coefci(mod, vcov = vcovHC(mod, type = "HC3"))
preds <- predictions(mod, newdata = datagrid(z_pyridge = joined$z_pyridge), vcov = vcovHC(mod, type = "HC3"))

#----------
# Draw plot
#----------

# Define palette

pals <- palette.colors(10, "Tableau 10")

mypal <- c("catch22" = pals[8],
           "feasts" = pals[7],
           "Kats" = pals[6],
           "tsfeatures" = pals[5],
           "TSFEL" = pals[4],
           "tsfresh" = pals[3])

# Draw plot

p <- joined |>
  ggplot(aes(x = z_pyridge, y = z_xgboost)) +
  geom_point(alpha = 0.8) +
  geom_ribbon(data = preds, aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = preds, aes(y = estimate), colour = "#3366FF", linewidth = 0.7) +
  #geom_smooth(aes(group = 1), formula = y ~ x, method = "lm", alpha = 0.3) +
  labs(subtitle = paste0("b = ", round(coefs[2], digits = 2), 
                         " (95% CI: ", round(coefs_ci[2], digits = 2), 
                         " - ", round(coefs_ci[4], digits = 2), ")"),
       x = "NPS (ridge logistic regression)",
       y = "NPS (XGBoost)",
       colour = NULL) +
  #scale_colour_manual(values = mypal) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p)
ggsave("output/pyridge-nps-vs-xgboost.pdf", p, units = "in", height = 6, width = 6)
