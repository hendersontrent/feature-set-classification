#------------------------------------------
# This script sets out to produce analysis
# of the distribution of normalised 
# performance scores as a violin plot
#
# NOTE: This script requires setup.R and
# analysis/compute-features.R and
# analysis/fit-classifiers.R to have been 
# run first
#-----------------------------------------

#--------------------------------------
# Author: Trent Henderson, 22 July 2022
#--------------------------------------

# Load classification results

load("data/outputs.Rda")

outputs <- outputs %>%
  mutate(method = case_when(
    method == "tsfel" ~ "TSFEL",
    method == "kats"  ~ "Kats",
    TRUE              ~ method))

# Filter to just problems present in tsfresh as it had some errors

tsfresh_probs <- outputs %>%
  filter(method == "tsfresh") %>%
  dplyr::select(c(problem)) %>%
  distinct() %>%
  pull()

#---------------------- Calculate scores--------------------

#------------------
# Overall benchmark
#------------------

# Calculate into own dataframe in case we want it later

benchmarks <- outputs %>%
  filter(problem %in% tsfresh_probs) %>%
  group_by(problem) %>%
  summarise(overall_avg = mean(balanced_accuracy, na.rm = TRUE),
            stddev = sd(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup()

#------------------------
# Individual feature sets
#------------------------

# Calculate z-scores

z_scores <- outputs %>%
  filter(problem %in% tsfresh_probs) %>%
  group_by(problem, method) %>%
  summarise(x = mean(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  inner_join(benchmarks, by = c("problem" = "problem")) %>%
  group_by(problem, method) %>%
  mutate(z = (x - overall_avg) / stddev) %>%
  ungroup()

z_scores <- z_scores %>%
  filter(problem %in% tsfresh_probs)

# Calculate global averages

avg_over_probs <- outputs %>%
  filter(problem %in% tsfresh_probs) %>%
  summarise(global_avg = mean(balanced_accuracy, na.rm = TRUE)) %>%
  pull()

set_averages <- outputs %>%
  filter(problem %in% tsfresh_probs) %>%
  group_by(method, problem) %>%
  summarise(global_avg = mean(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup()

#---------------------- Draw graphic -----------------------

set.seed(123)

p <- z_scores %>%
  inner_join(set_averages, by = c("method" = "method", "problem" = "problem")) %>%
  ggplot(aes(x = reorder(method, -global_avg), y = z, colour = method)) +
  geom_violin() +
  geom_hline(yintercept = 0, lty = "dashed", colour = "black") +
  #geom_point(size = 0.7, alpha = 0.9, position = ggplot2::position_jitter(w = 0.05)) +
  geom_point(size = 0.7, alpha = 0.9) +
  #geom_line(aes(group = problem), colour = "grey50", size = 0.3, alpha = 0.5) +
  labs(title = "Distributions of z-score accuracy across UEA/UCR repository univariate problems",
       subtitle = "Performance scores calculated relative to mean and SD across all sets for each problem.",
       x = "Feature set",
       y = "Normalised performance score",
       colour = NULL,
       caption = "Value of 0 indicates no difference from the mean. Value of |1| indicates 1 standard deviation away from mean.") +
  scale_colour_brewer(palette = "Dark2") +
  theme_bw() +
  theme(legend.position = "none")

print(p)
ggsave("output/non-z-scored/normalised-performance-score-distribution.pdf", p)
