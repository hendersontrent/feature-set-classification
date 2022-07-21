#------------------------------------------
# This script sets out to produce analysis
# of feature set performance against bake-off
# paper benchmarks
#
# NOTE: This script requires setup.R and
# analysis/compute-features.R and
# analysis/fit-classifiers.R to have been 
# run first
#-----------------------------------------

#-------------------------------------
# Author: Trent Henderson, 5 July 2022
#-------------------------------------

# Grab benchmark results

benchmarks <- pull_benchmark_results() %>%
  filter(method != "catch22") %>%
  dplyr::select(-c(balanced_accuracy)) %>%
  mutate(accuracy_bench = accuracy * 100) %>%
  group_by(problem) %>%
  mutate(ranker = dense_rank(-accuracy_bench)) %>%
  ungroup() %>%
  filter(ranker == 1) %>%
  dplyr::select(c(method, problem, accuracy_bench)) %>%
  rename(method_bench = method)

 # Load classification results

load("data/outputs.Rda")
load("data/outputs_aggregate.Rda")

# Concatenate the three datasets

outputs <- outputs %>%
  dplyr::select(c(method, problem, accuracy))

outputs_aggregate <- outputs_aggregate %>%
  mutate(method = "All features") %>%
  dplyr::select(c(method, problem, accuracy))

main_models <- bind_rows(outputs, outputs_aggregate) %>%
  group_by(problem, method) %>%
  summarise(accuracy = mean(accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(accuracy = accuracy * 100) %>%
  group_by(problem) %>%
  mutate(ranker = dense_rank(-accuracy)) %>%
  ungroup() %>%
  filter(ranker == 1) %>%
  dplyr::select(-c(ranker))

main_models <- main_models %>%
  inner_join(benchmarks, by = c("problem" = "problem")) %>%
  mutate(method = case_when(
          method == "tsfel" ~ "TSFEL",
          method == "kats"  ~ "Kats",
          TRUE              ~ method)) %>%
  mutate(top_performer = ifelse(accuracy > accuracy_bench, method, method_bench))

rm(benchmarks, outputs, outputs_aggregate)

#--------------------- Draw plots ------------------

# Create palette for whoever is top performer

mypal <- c("All Features" = "grey50",
           "TS-CHIEF" = "black",
           "HIVE-COTE v1.0" = "black",
           "ROCKET" = "black",
           "InceptionTime" = "black",
           "STC" = "black",
           "ResNet" = "black",
           "ProximityForest" = "black",
           "WEASEL" = "black",
           "S-BOSS" = "black",
           "cBOSS" = "black",
           "BOSS" = "black",
           "RISE" = "black",
           "TSF" = "black",
           "catch22" = "#1B9E77",
           "feasts" = "#D95F02",
           "Kats" = "#7570B3",
           "tsfeatures" = "#E7298A",
           "TSFEL" = "#66A61E",
           "tsfresh" = "#E6AB02")

myshapes <- c("All Features" = 16,
              "TS-CHIEF" = 0,
              "HIVE-COTE v1.0" = 1,
              "ROCKET" = 2,
              "InceptionTime" = 3,
              "STC" = 4,
              "ResNet" = 5,
              "ProximityForest" = 6,
              "WEASEL" = 7,
              "S-BOSS" = 8,
              "cBOSS" = 9,
              "BOSS" = 10,
              "RISE" = 11,
              "TSF" = 12,
              "catch22" = 16,
              "feasts" = 16,
              "Kats" = 16,
              "tsfeatures" = 16,
              "TSFEL" = 16,
              "tsfresh" = 16)

# Define coordinates for upper triangle to shade

upper_tri <- data.frame(x = c(0, 0, 100), y = c(0, 100, 100))

# Draw scatterplot

p <- main_models %>%
  ggplot(aes(x = accuracy, y = accuracy_bench)) +
  geom_polygon(data = upper_tri, aes(x = x, y = y), fill = "steelblue2", alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, colour = "grey50", lty = "dashed") +
  #geom_errorbar(aes(ymin = lower_y, ymax = upper_y, colour = top_performer)) +
  #geom_errorbarh(aes(xmin = lower_x, xmax = upper_x, colour = top_performer)) +
  geom_point(aes(colour = top_performer, shape = top_performer), size = 2) +
  annotate("text", x = 80, y = 10, label = "Time-series features better") +
  annotate("text", x = 20, y = 90, label = "Leading benchmark better") +
  labs(title = "Comparison of feature sets versus benchmark algorithms across UCR/UEA repository univariate problems",
       subtitle = "Plots a subset of 54 problems that preliminary analysis showed mean and variance did not outperform chance",
       x = "Classification accuracy time-series features (%)",
       y = "Classification accuracy benchmark algorithm (%)",
       colour = NULL,
       shape = NULL) +
  scale_x_continuous(labels = function(x)paste0(x, "%")) + 
  scale_y_continuous(labels = function(x)paste0(x, "%")) + 
  scale_colour_manual(values = mypal,
                      name = "Algorithm and Set") +
  scale_shape_manual(values = myshapes,
                     name = "Algorithm and Set") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank())

print(p)
ggsave("output/non-z-scored/features-vs-bench.pdf", p)
