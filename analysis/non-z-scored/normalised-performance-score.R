#------------------------------------------
# This script sets out to produce analysis
# of normalised performance scores
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

outputs <- outputs %>%
  mutate(method = case_when(
    method == "tsfel" ~ "TSFEL",
    method == "kats"  ~ "Kats",
    TRUE              ~ method))

#---------------------- Calculate scores--------------------

#------------------
# Overall benchmark
#------------------

# Calculate into own dataframe in case we want it later

benchmarks <- outputs %>%
  group_by(problem) %>%
  summarise(overall_avg = mean(balanced_accuracy, na.rm = TRUE),
            stddev = sd(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup()

#------------------------
# Individual feature sets
#------------------------

# Calculate z-scores

z_scores <- outputs %>%
  group_by(problem, method) %>%
  summarise(x = mean(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  inner_join(benchmarks, by = c("problem" = "problem")) %>%
  group_by(problem, method) %>%
  mutate(z = (x - overall_avg) / stddev) %>%
  ungroup()

# Filter to just problems present in tsfresh as it had some errors

tsfresh_probs <- z_scores %>%
  filter(method == "tsfresh") %>%
  dplyr::select(c(problem)) %>%
  distinct() %>%
  pull()

z_scores <- z_scores %>%
  filter(problem %in% tsfresh_probs)

#---------------------- Draw graphic -----------------------

p <- z_scores %>%
  ggplot(aes(x = method, y = problem, fill = z)) +
  geom_tile() +
  labs(title = "Comparison of z-score accuracy across UEA/UCR repository univariate problems",
       subtitle = "Performance scores calculated relative to mean and SD across all sets for each problem",
       x = "Feature set",
       y = "Problem",
       fill = "Normalised performance score",
       caption = "Value of 0 indicates no difference from the mean. Value of |1| indicates 1 standard deviation away from mean.") +
  scale_fill_viridis_c() +
  #scale_fill_fermenter(palette = "RdBu") +
  theme_bw() +
  theme(legend.position = "bottom")

print(p)
ggsave("output/non-z-scored/normalised-performance-score.pdf", p)
