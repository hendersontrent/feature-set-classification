#------------------------------------------
# This script sets out to produce analysis
# of normalised performance scores
#
# NOTE: This script requires setup.R and
# analysis/compute-features_z-score.R and
# analysis/fit-classifiers_z-score.R to have 
# been run first
#-----------------------------------------

#--------------------------------------
# Author: Trent Henderson, 21 July 2022
#--------------------------------------

# Load classification results

load("data/outputs_z.Rda")

outputs_z <- outputs_z %>%
  mutate(method = case_when(
    method == "tsfel" ~ "TSFEL",
    method == "kats"  ~ "Kats",
    TRUE              ~ method))

#---------------------- Calculate scores--------------------

#------------------
# Overall benchmark
#------------------

# Calculate into own dataframe in case we want it later

benchmarks <- outputs_z %>%
  group_by(problem) %>%
  summarise(overall_avg = mean(balanced_accuracy, na.rm = TRUE),
            stddev = sd(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup()

#------------------------
# Individual feature sets
#------------------------

# Calculate z-scores

z_scores <- outputs_z %>%
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

benchmarks_sets <- outputs_z %>%
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
  labs(x = "Feature set",
       y = "Problem",
       fill = "Normalised performance score") +
  scale_fill_gradientn(colours = c("#0571B0", "#92C5DE", "white", "white", "white", "#F4A582", "#CA0020"),
                       breaks = c(-2.5, -2, -1, -0.5, 0, 0.5, 1, 2, 2.5),
                       labels = c(-2.5, -2, -1, -0.5, 0, 0.5, 1, 2, 2.5),
                       limits = c(-2.5, 2.5)) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        panel.grid = element_blank())

print(p)
ggsave("output/z-scored/normalised-performance-score.pdf", p, units = "in", height = 11, width = 11)
