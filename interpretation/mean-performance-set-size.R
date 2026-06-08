#---------------------------------------
# This script draws a plot of performance
# and feature set size
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 8 June 2026
#---------------------------------------

library(dplyr)
library(ggplot2)
library(scales)
library(ggrepel)

#--------------- Read in data ---------------

files <- list.files("classification-models/results/svm/")
accuracies <- vector(mode = "list", length = length(files))

for(i in files){
  feature_sets <- read.csv(paste0("classification-models/results/svm/", i))
  accuracies[[match(i, files)]] <- feature_sets
}

accuracies <- do.call("rbind", accuracies) |>
  filter(feature_set != "timegp") # From another related project which used the same methodology

rm(files, i, feature_sets)

#--------------- Draw plot ---------------

# Set up palette

pals <- palette.colors(10, "Tableau 10")

mypal <- c("catch22" = pals[8],
           "feasts" = pals[7],
           "Kats" = pals[6],
           "tsfeatures" = pals[5],
           "TSFEL" = pals[4],
           "tsfresh" = pals[3])

# Draw plot

p <- accuracies |>
  mutate(n_features = case_when(
    feature_set == "catch22"    ~ 22,
    feature_set == "feasts"     ~ 43,
    feature_set == "tsfeatures" ~ 62,
    feature_set == "Kats"       ~ 40,
    feature_set == "TSFEL"      ~ 156,
    feature_set == "tsfresh"    ~ 783)) |> # Add feature set size
  mutate(my_label = paste0(n_features, " features")) |>
  ggplot(aes(x = n_features, y = accuracy, colour = feature_set)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 10, linewidth = 0.7, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", size = 5) +
  stat_summary(fun = mean, geom = "label_repel", aes(label = my_label),
               fontface = "bold", show.legend = FALSE, size = 6) +
  labs(x = "Number of features",
       y = "Mean classification accuracy (%)",
       colour = NULL) +
  scale_y_continuous(labels = percent) +
  scale_colour_manual(values = mypal) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 16))

print(p)
ggsave("output/set-size-mean-performance.pdf", p, units = "in", height = 8, width = 8)
