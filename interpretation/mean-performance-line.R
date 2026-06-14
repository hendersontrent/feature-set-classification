#-------------------------------------
# This script sets out to plot overall
# performance for each set across 
# problems
#-------------------------------------

#------------------------------------
# Author: Trent Henderson, 5 May 2026
#------------------------------------

library(dplyr)
library(scales)
library(ggplot2)

# Read in data

files <- list.files(paste0("classification-models/results/xgboost"))
accuracies <- vector(mode = "list", length = length(files))

for(i in files){
  accuracies[[match(i, files)]] <- read.csv(paste0("classification-models/results/xgboost/", i))
}

accuracies <- do.call("rbind", accuracies) |>
  filter(feature_set %in% c("catch22", "feasts", "tsfeatures", "Kats", 
                            "TSFEL", "tsfresh", "FFT", "FFT (Mag^2 + Angle) + quantiles"))

#---------------------- Calculations ----------------------

calcs <- accuracies |>
  reframe(.mean = mean(accuracy, na.rm = TRUE),
          .sd = sd(accuracy, na.rm = TRUE),
          .by = c("problem", "feature_set"))

# Order for plot

orders <- accuracies |>
  reframe(.mean = mean(accuracy, na.rm = TRUE), .by = "problem") |>
  mutate(orders = dense_rank(.mean)) |>
  dplyr::select(c(problem, orders))

calcs <- calcs |>
  left_join(orders, by = c("problem" = "problem"))

# Case studies to highlight

case_studies <- calcs |>
  mutate(flag = case_when(
    problem == "SyntheticControl" & feature_set == "Kats"  ~ TRUE,
    problem == "TwoPatterns" & feature_set == "Kats"       ~ TRUE,
    problem == "FaceFour" & feature_set == "tsfresh"       ~ TRUE)) |>
  filter(flag) |>
  mutate(.mean = .mean * 100)

#---------------------- Plotting ----------------------

pals <- palette.colors(10, "Tableau 10")

mypal <- c("catch22" = pals[8],
           "feasts" = pals[7],
           "Kats" = pals[6],
           "tsfeatures" = pals[5],
           "TSFEL" = pals[4],
           "tsfresh" = pals[3],
           "FFT (Mag^2 + Angle) + quantiles" = "black")

p <- calcs |>
  mutate(.mean = .mean * 100) |>
  mutate(feature_set = factor(feature_set, levels = c("catch22", "feasts", "Kats",
                                                      "tsfeatures", "TSFEL", "tsfresh",
                                                      "FFT (Mag^2 + Angle) + quantiles"))) |>
  ggplot(aes(x = reorder(problem, -orders), y = .mean, group = feature_set, colour = feature_set)) +
  geom_line(linewidth = 0.8) +
  geom_point(data = case_studies, size = 5, show.legend = FALSE) +
  labs(x = "Problem",
       y = "Mean classification accuracy (%)",
       colour = NULL) +
  scale_y_continuous(labels = function(x)paste0(x, "%")) + 
  scale_colour_manual(values = mypal,
                      labels = function(x) {
                        x <- gsub("Mag^2", "Mag²", x, fixed = TRUE)
                      }) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11))

print(p)
ggsave("output/mean-performance-line-plot.pdf", p, unit = "in", width = 16, height = 10)

#---------------------- Compute variance statistics ----------------------

# Variance of feature sets within problems

accuracies |>
  reframe(.mean = mean(accuracy, na.rm = TRUE), .by = c("problem", "feature_set")) |>
  reframe(.var = var(.mean), .by = "problem") |>
  reframe(.mean = mean(.var))

# Variance of feature sets between problems

accuracies |>
  reframe(.mean = mean(accuracy, na.rm = TRUE), .by = c("feature_set", "problem")) |>
  reframe(.var = var(.mean), .by = "feature_set") |>
  reframe(.mean = mean(.var))

#---------------------- Calculate case study differentials ----------------------

case_studies_diffs <- accuracies |>
  mutate(flag = case_when(
    problem == "SyntheticControl" & feature_set == "Kats"  ~ TRUE,
    problem == "TwoPatterns" & feature_set == "Kats"       ~ TRUE,
    problem == "FaceFour" & feature_set == "tsfresh"       ~ TRUE,
    TRUE                                                   ~ FALSE)) |>
  filter(problem %in% c("SyntheticControl", "TwoPatterns", "FaceFour", "EthanolLevel", "Wine")) |>
  reframe(.mean = mean(accuracy, na.rm = TRUE), .by = c("problem", "flag"))
