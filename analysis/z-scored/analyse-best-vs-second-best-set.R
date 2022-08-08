#------------------------------------------
# This script sets out to produce analysis
# of best versus second best feature set by 
# problem
#
# NOTE: This script requires setup.R and
# analysis/compute-features.R and
# analysis/fit-classifiers.R to have been 
# run first
#-----------------------------------------

#--------------------------------------
# Author: Trent Henderson, 25 July 2022
#--------------------------------------

# Load classification results

load("data/outputs_z.Rda")

outputs <- outputs_z %>%
  mutate(method = case_when(
    method == "tsfel" ~ "TSFEL",
    method == "kats"  ~ "Kats",
    TRUE              ~ method))

# Find best feature set by problem

best <- outputs_z %>%
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

best_2 <- outputs_z %>%
  mutate(balanced_accuracy = balanced_accuracy * 100) %>%
  group_by(problem, method) %>%
  summarise(balanced_accuracy_mean = mean(balanced_accuracy, na.rm = TRUE),
            balanced_accuracy_sd = sd(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(problem) %>%
  mutate(ranker = dense_rank(-balanced_accuracy_mean)) %>%
  ungroup() %>%
  filter(ranker == 2) %>%
  dplyr::select(-c(ranker)) %>%
  rename(worst_method = method,
         worst_balanced_accuracy_mean = balanced_accuracy_mean,
         worst_balanced_accuracy_sd = balanced_accuracy_sd)

both <- best %>%
  inner_join(best_2, by = c("problem" = "problem")) %>%
  mutate(lower_x = worst_balanced_accuracy_mean - 1 * worst_balanced_accuracy_sd,
         upper_x = worst_balanced_accuracy_mean + 1 * worst_balanced_accuracy_sd,
         lower_y = best_balanced_accuracy_mean - 1 * best_balanced_accuracy_sd,
         upper_y = best_balanced_accuracy_mean + 1 * best_balanced_accuracy_sd)

#---------------------- Calculate p-values -----------------------

p_values <- unique(both$problem) %>%
  purrr::map_df(~ calculate_p_values(data = outputs, summary_data = both, theproblem = .x, all_features = FALSE))

both <- both %>%
  inner_join(p_values, by = c("problem" = "problem")) %>%
  mutate(significant = ifelse(p_value < 0.05, "Significant difference", "Non-significant difference"),
         top_performer = ifelse(significant == "Significant difference", best_method, "Non-Significant difference"))

rm(outputs, best, best_2)

#---------------------- Draw summary graphic ---------------------

# Create palette for whoever is top performer

mypal <- c("catch22" = "#1B9E77",
           "feasts" = "#D95F02",
           "Kats" = "#7570B3",
           "tsfeatures" = "#E7298A",
           "TSFEL" = "#66A61E",
           "tsfresh" = "#E6AB02",
           "Non-Significant difference" = "grey50")

# Define coordinates for upper triangle to shade

upper_tri <- data.frame(x = c(0, 0, 100), y = c(0, 100, 100))

# Draw scatterplot

p <- both %>%
  ggplot(aes(x = worst_balanced_accuracy_mean, y = best_balanced_accuracy_mean)) +
  geom_polygon(data = upper_tri, aes(x = x, y = y), fill = "steelblue2", alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, colour = "grey50", lty = "dashed") +
  geom_errorbar(aes(ymin = lower_y, ymax = upper_y, colour = top_performer)) +
  geom_errorbarh(aes(xmin = lower_x, xmax = upper_x, colour = top_performer)) +
  geom_point(aes(colour = top_performer), size = 2) +
  annotate("text", x = 75, y = 10, label = "Second best feature set") +
  annotate("text", x = 25, y = 90, label = "Best feature set") +
  labs(title = "Comparison of best and second best feature sets across UCR/UEA repository univariate problems",
       subtitle = "Error bars are +/- 1 SD obtained over 30 resamples. Colour indicates p < .05 difference",
       x = "Balanced classification accuracy second best set (%)",
       y = "Balanced classification accuracy best set (%)",
       colour = "Best feature set") +
  scale_x_continuous(labels = function(x)paste0(x, "%")) + 
  scale_y_continuous(labels = function(x)paste0(x, "%")) + 
  scale_colour_manual(values = mypal) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

print(p)
ggsave("output/z-scored/best_versus_second_best_set.pdf", p)
