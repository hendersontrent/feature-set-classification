#------------------------------------------
# This script sets out to produce analysis
# of classification performance for sets
# versus all features at once
#
# NOTE: This script requires setup.R and
# analysis/compute-features.R and
# analysis/fit-classifiers.R to have been 
# run first
#-----------------------------------------

#-------------------------------------
# Author: Trent Henderson, 14 May 2022
#-------------------------------------

# Load classification results

load("data/outputs_z.Rda")
load("data/outputs_aggregate_z.Rda")

# Run the function

main_models <- outputs_z %>%
  mutate(accuracy = accuracy * 100,
         balanced_accuracy = balanced_accuracy * 100) %>%
  group_by(problem, method) %>%
  summarise(balanced_accuracy_mean = mean(balanced_accuracy, na.rm = TRUE),
            balanced_accuracy_sd = sd(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(method_set = method) %>%
  group_by(problem) %>%
  mutate(ranker = dense_rank(-balanced_accuracy_mean)) %>%
  ungroup() %>%
  filter(ranker == 1) %>%
  dplyr::select(-c(ranker)) %>%
  rename(balanced_accuracy = balanced_accuracy_mean)

main_models_aggregate <- outputs_aggregate_z %>%
  mutate(accuracy = accuracy * 100,
         balanced_accuracy = balanced_accuracy * 100,
         method = "All Features") %>%
  group_by(problem, method) %>%
  summarise(balanced_accuracy_all = mean(balanced_accuracy, na.rm = TRUE),
            balanced_accuracy_sd_all = sd(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup()

# Join the datasets and compute bars for plot + the top overall performer

all_mains <- main_models %>%
  left_join(main_models_aggregate, by = c("problem" = "problem")) %>%
  mutate(lower_x = balanced_accuracy - 1 * balanced_accuracy_sd,
         upper_x = balanced_accuracy + 1 * balanced_accuracy_sd,
         lower_y = balanced_accuracy_all - 1 * balanced_accuracy_sd_all,
         upper_y = balanced_accuracy_all + 1 * balanced_accuracy_sd_all) %>%
  mutate(top_performer = ifelse(balanced_accuracy > balanced_accuracy_all, method_set, method),
         method = case_when(
           method == "tsfel" ~ "TSFEL",
           method == "kats"  ~ "Kats",
           TRUE              ~ method)) %>%
  drop_na()

#------------------ Analysis I: Top performer per problem -----------------

# Create palette for whoever is top performer

mypal <- c("All Features" = "black",
           "catch22" = "#1B9E77",
           "feasts" = "#D95F02",
           "Kats" = "#7570B3",
           "tsfeatures" = "#E7298A",
           "TSFEL" = "#66A61E",
           "tsfresh" = "#E6AB02")

# Define coordinates for upper triangle to shade

upper_tri <- data.frame(x = c(0, 0, 100), y = c(0, 100, 100))

# Draw scatterplot

p <- all_mains %>%
  ggplot(aes(x = balanced_accuracy, y = balanced_accuracy_all)) +
  geom_polygon(data = upper_tri, aes(x = x, y = y), fill = "steelblue2", alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, colour = "grey50", lty = "dashed") +
  geom_errorbar(aes(ymin = lower_y, ymax = upper_y, colour = top_performer)) +
  geom_errorbarh(aes(xmin = lower_x, xmax = upper_x, colour = top_performer)) +
  geom_point(aes(colour = top_performer), size = 2) +
  annotate("text", x = 75, y = 10, label = "Single feature set better") +
  annotate("text", x = 25, y = 90, label = "All features better") +
  labs(title = "Comparison of top feature sets across UCR/UEA repository univariate problems",
       subtitle = "Error bars are +/- 1 SD obtained over 30 resamples",
       x = "Balanced classification accuracy individual set (%)",
       y = "Balanced classification accuracy all features (%)",
       colour = NULL) +
  scale_x_continuous(labels = function(x)paste0(x, "%")) + 
  scale_y_continuous(labels = function(x)paste0(x, "%")) + 
  scale_colour_manual(values = mypal) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

print(p)
ggsave("output/z-scored/all_versus_sets.pdf", p)
