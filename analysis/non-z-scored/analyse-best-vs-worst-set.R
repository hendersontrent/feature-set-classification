#------------------------------------------
# This script sets out to produce analysis
# of best versus worst feature set by 
# problem
#
# NOTE: This script requires setup.R and
# analysis/compute-features.R and
# analysis/fit-classifiers.R to have been 
# run first
#-----------------------------------------

#--------------------------------------
# Author: Trent Henderson, 21 July 2022
#--------------------------------------

# Load classification results

load("data/outputs.Rda")

# Find best feature set by problem

best <- outputs %>%
  mutate(balanced_accuracy = balanced_accuracy * 100) %>%
  group_by(problem, method) %>%
  summarise(best_balanced_accuracy_mean = mean(balanced_accuracy, na.rm = TRUE),
            best_balanced_accuracy_sd = sd(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(problem) %>%
  mutate(ranker = dense_rank(-best_balanced_accuracy_mean)) %>%
  ungroup() %>%
  filter(ranker == 1) %>%
  dplyr::select(-c(ranker)) %>%
  rename(best_method = method)

# Find worst feature set by problem

worst <- outputs %>%
  mutate(balanced_accuracy = balanced_accuracy * 100) %>%
  group_by(problem, method) %>%
  summarise(balanced_accuracy_mean = mean(balanced_accuracy, na.rm = TRUE),
            balanced_accuracy_sd = sd(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(problem) %>%
  mutate(ranker = dense_rank(-balanced_accuracy_mean)) %>%
  ungroup() %>%
  filter(ranker == 6) %>%
  dplyr::select(-c(ranker)) %>%
  rename(worst_method = method,
         worst_balanced_accuracy_mean = balanced_accuracy_mean,
         worst_balanced_accuracy_sd = balanced_accuracy_sd)

both <- best %>%
  inner_join(worst, by = c("problem" = "problem")) %>%
  mutate(best_method = case_when(
    best_method == "tsfel" ~ "TSFEL",
    best_method == "kats"  ~ "Kats",
    TRUE                   ~ best_method)) %>%
  mutate(worst_method = case_when(
    worst_method == "tsfel" ~ "TSFEL",
    worst_method == "kats"  ~ "Kats",
    TRUE                    ~ worst_method)) %>%
  mutate(lower_x = worst_balanced_accuracy_mean - 1 * worst_balanced_accuracy_sd,
         upper_x = worst_balanced_accuracy_mean + 1 * worst_balanced_accuracy_sd,
         lower_y = best_balanced_accuracy_mean - 1 * best_balanced_accuracy_sd,
         upper_y = best_balanced_accuracy_mean + 1 * best_balanced_accuracy_sd)

rm(outputs, best, worst)

#---------------------- Draw summary graphic ---------------------

# Create palette for whoever is top performer

mypal <- c("catch22" = "#1B9E77",
           "feasts" = "#D95F02",
           "Kats" = "#7570B3",
           "tsfeatures" = "#E7298A",
           "TSFEL" = "#66A61E",
           "tsfresh" = "#E6AB02")

# Define coordinates for upper triangle to shade

upper_tri <- data.frame(x = c(0, 0, 100), y = c(0, 100, 100))

# Draw scatterplot

p <- both %>%
  ggplot(aes(x = worst_balanced_accuracy_mean, y = best_balanced_accuracy_mean)) +
  geom_polygon(data = upper_tri, aes(x = x, y = y), fill = "steelblue2", alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, colour = "grey50", lty = "dashed") +
  geom_errorbar(aes(ymin = lower_y, ymax = upper_y, colour = best_method)) +
  geom_errorbarh(aes(xmin = lower_x, xmax = upper_x, colour = best_method)) +
  geom_point(aes(colour = best_method), size = 2) +
  annotate("text", x = 75, y = 10, label = "Best feature set") +
  annotate("text", x = 25, y = 90, label = "Worst feature set") +
  labs(title = "Comparison of best and worst feature sets across UCR/UEA repository univariate problems",
       subtitle = "Error bars are +/- 1 SD obtained over 30 resamples",
       x = "Balanced classification accuracy worst set (%)",
       y = "Balanced classification accuracy best set (%)",
       colour = "Best feature set") +
  scale_x_continuous(labels = function(x)paste0(x, "%")) + 
  scale_y_continuous(labels = function(x)paste0(x, "%")) + 
  scale_colour_manual(values = mypal) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

print(p)
ggsave("output/non-z-scored/all_versus_sets.pdf", p)
