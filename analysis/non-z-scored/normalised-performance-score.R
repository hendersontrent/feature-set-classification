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

# Mean accuracy by set

benchmarks_sets <- outputs %>%
  filter(problem %in% tsfresh_probs) %>%
  group_by(method) %>%
  summarise(global_avg = mean(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup()

#---------------------- Cluster by problem -----------------

# Hierarchical cluster on rows (problems)

z_scores_mat <- z_scores %>%
  dplyr::select(c(problem, method, z)) %>%
  pivot_wider(id_cols = "problem", names_from = "method", values_from = "z") %>%
  tibble::column_to_rownames(var = "problem")

row.order <- stats::hclust(stats::dist(z_scores_mat, method = "euclidean"), method = "average")$order
z_scores_mat <- z_scores_mat[row.order, ]

z_scores_mat <- reshape2::melt(as.matrix(z_scores_mat)) %>%
  rename(problem = Var1,
         method = Var2) %>%
  left_join(benchmarks_sets, by = c("method" = "method"))

#---------------------- Draw graphic -----------------------

p <- z_scores_mat %>%
  ggplot(aes(x = reorder(method, -global_avg), y = problem, fill = value)) +
  geom_tile() +
  labs(title = "Comparison of z-score accuracy across UEA/UCR repository univariate problems",
       subtitle = "Performance scores calculated relative to mean and SD across all sets for each problem.\nColumns organised by descending overall mean balanced accuracy.\nRows organised by heirarchical clustering.",
       x = "Feature set",
       y = "Problem",
       fill = "Normalised performance score",
       caption = "Value of 0 indicates no difference from the mean. Value of |1| indicates 1 standard deviation away from mean.") +
  scale_fill_gradient2(low = "#0571B0",
                       mid = "white",
                       high = "#CA0020",
                       midpoint = 0) +
  theme_bw() +
  theme(legend.position = "bottom")

print(p)
ggsave("output/non-z-scored/normalised-performance-score.pdf", p)
