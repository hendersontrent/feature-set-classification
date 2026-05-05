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

files <- list.files(paste0("classification-models/results/svm"))
accuracies <- vector(mode = "list", length = length(files))

for(i in files){
  accuracies[[match(i, files)]] <- read.csv(paste0("classification-models/results/svm/", i))
}

accuracies <- do.call("rbind", accuracies) |>
  filter(feature_set != "timegp") # From another related project which used the same methodology

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
    problem == "Beef" & feature_set == "tsfresh"           ~ TRUE,
    problem == "EthanolLevel" & feature_set == "tsfresh"   ~ TRUE,
    problem == "Wine" & feature_set == "catch22"           ~ TRUE)) |>
  filter(flag) |>
  mutate(.mean = .mean * 100)

#---------------------- Plotting ----------------------

pals <- palette.colors(10, "Tableau 10")

mypal <- c("catch22" = pals[8],
           "feasts" = pals[7],
           "Kats" = pals[6],
           "tsfeatures" = pals[5],
           "TSFEL" = pals[4],
           "tsfresh" = pals[3])

p <- calcs |>
  mutate(.mean = .mean * 100) |>
  ggplot(aes(x = reorder(problem, -orders), y = .mean, group = feature_set, colour = feature_set)) +
  geom_line(linewidth = 0.8) +
  geom_point(data = case_studies, size = 5, show.legend = FALSE) +
  labs(x = "Problem",
       y = "Mean classification accuracy (%)",
       colour = NULL) +
  scale_y_continuous(labels = function(x)paste0(x, "%")) + 
  scale_colour_manual(values = mypal) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11))

print(p)
ggsave("output/mean-performance-line-plot.pdf", p, unit = "in", width = 16, height = 10)

#---------------------- Compute proportion within 10% of best ----------------------

best <- calcs |>
  group_by(problem) |>
  filter(.mean == max(.mean)) |>
  ungroup() |>
  dplyr::select(c(problem, .mean)) |>
  rename(best_mean = .mean)

worst <- calcs |>
  group_by(problem) |>
  filter(.mean == min(.mean)) |>
  ungroup() |>
  dplyr::select(c(problem, .mean)) |>
  rename(worst_mean = .mean)

both <- best |>
  inner_join(worst, by = "problem") |>
  distinct() |>
  mutate(pc_10_bound = best_mean - (best_mean * 0.1)) |>
  mutate(flag = ifelse(worst_mean > pc_10_bound, "Within 10%", "Not within 10%"))

both |>
  reframe(counter = n(), .by = "flag") |>
  mutate(props = counter / sum(counter) * 100)
