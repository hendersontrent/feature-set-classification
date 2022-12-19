#------------------------------------------
# This script sets out to produce analysis
# of the distribution of normalised 
# performance scores as a violin plot
#
# NOTE: This script requires setup.R and
# analysis/compute-features_z-score.R and
# analysis/fit-classifiers_z-score.R to have 
# been run first
#-----------------------------------------

#--------------------------------------
# Author: Trent Henderson, 22 July 2022
#--------------------------------------

# Load classification results

load("data/outputs_z.Rda")

outputs_z <- outputs_z %>%
  mutate(method = case_when(
    method == "tsfel" ~ "TSFEL",
    method == "kats"  ~ "Kats",
    TRUE              ~ method))

# Filter to just problems present in tsfresh as it had some errors

tsfresh_probs <- outputs_z %>%
  filter(method == "tsfresh") %>%
  dplyr::select(c(problem)) %>%
  distinct() %>%
  pull()

#---------------------- Calculate scores--------------------

#------------------
# Overall benchmark
#------------------

# Calculate into own dataframe in case we want it later

benchmarks <- outputs_z %>%
  filter(problem %in% tsfresh_probs) %>%
  group_by(problem) %>%
  summarise(overall_avg = mean(balanced_accuracy, na.rm = TRUE),
            stddev = sd(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup()

#------------------------
# Individual feature sets
#------------------------

# Calculate z-scores

z_scores <- outputs_z %>%
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

avg_over_probs <- outputs_z %>%
  filter(problem %in% tsfresh_probs) %>%
  summarise(global_avg = mean(balanced_accuracy, na.rm = TRUE)) %>%
  pull()

set_averages <- outputs_z %>%
  filter(problem %in% tsfresh_probs) %>%
  group_by(method, problem) %>%
  summarise(global_avg = mean(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup()

#---------------------- Draw graphic -----------------------

set.seed(123)

mypal <- c("catch22" =  mypal[1],
           "feasts" = mypal[2],
           "Kats" = mypal[3],
           "tsfeatures" = mypal[4],
           "TSFEL" = mypal[5],
           "tsfresh" = mypal[6])

p <- z_scores %>%
  inner_join(set_averages, by = c("method" = "method", "problem" = "problem")) %>%
  ggplot(aes(x = reorder(method, -global_avg), y = z, colour = method)) +
  geom_violin() +
  geom_hline(yintercept = 0, lty = "dashed", colour = "black") +
  geom_jitter(size = 0.7, alpha = 0.9, height = 0, width = 0.1) +
  labs(x = "Feature set",
       y = "Normalised performance score",
       colour = NULL) +
  scale_colour_manual(values = mypal) +
  theme_bw() +
  theme(legend.position = "none")

print(p)
ggsave("output/z-scored/normalised-performance-score-distribution.pdf", p)
