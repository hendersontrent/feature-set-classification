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

#---------------------- Calculate scores--------------------

#------------------
# Overall benchmark
#------------------

# Calculate into own dataframe in case we want it later

benchmarks <- outputs_z %>%
  group_by(problem) %>%
  summarise(overall_avg = mean(accuracy, na.rm = TRUE),
            stddev = sd(accuracy, na.rm = TRUE)) %>%
  ungroup()

#------------------------
# Individual feature sets
#------------------------

# Calculate z-scores

z_scores <- outputs_z %>%
  group_by(problem, method) %>%
  summarise(x = mean(accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  inner_join(benchmarks, by = c("problem" = "problem")) %>%
  group_by(problem, method) %>%
  mutate(z = (x - overall_avg) / stddev) %>%
  ungroup()

# Mean accuracy by set

benchmarks_sets <- outputs_z %>%
  group_by(method) %>%
  summarise(global_avg = mean(accuracy, na.rm = TRUE)) %>%
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

# Main plot

p <- z_scores_mat %>%
  ggplot(aes(x = reorder(method, -global_avg), y = problem, fill = value)) +
  geom_tile() +
  # geom_hline(yintercept = 98.5, lty = "solid", colour = "black", size = 0.7) +
  # geom_hline(yintercept = 89.5, lty = "solid", colour = "black", size = 0.7) +
  # geom_hline(yintercept = 64.5, lty = "solid", colour = "black", size = 0.7) +
  # geom_hline(yintercept = 43.5, lty = "solid", colour = "black", size = 0.7) +
  # geom_hline(yintercept = 7.5, lty = "solid", colour = "black", size = 0.7) +
  # geom_hline(yintercept = 4.5, lty = "solid", colour = "black", size = 0.7) +
  # geom_hline(yintercept = 2.5, lty = "solid", colour = "black", size = 0.7) +
  labs(x = "Feature set",
       y = "Problem",
       fill = "Normalised performance score") +
  scale_fill_gradientn(colours = c("#0571B0", "#92C5DE", "white", "white", "white", "#F4A582", "#CA0020"),
                       breaks = c(-2.5, -2, -1, -0.5, 0, 0.5, 1, 2, 2.5),
                       labels = c(-2.5, -2, -1, -0.5, 0, 0.5, 1, 2, 2.5),
                       limits = c(-2.5, 2.5)) +
  theme_bw() +
  coord_cartesian(xlim = c(1, 6), clip = "off") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        panel.grid = element_blank(),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11))

# Side annotations

label_data <- data.frame(x = rep(0.5, times = 6),
                         y = c(-3, 2, 25, 52, 78, 97),
                         mylab = c("vi) Strong TSFEL performance", "v) Poor Kats performance", "iv) Other", "iii) Strong tsfresh performance", "ii) Other", "i) Strong tsfresh performance"))

ann <- ggplot(data = label_data) +
  geom_text(aes(x = x, y = y, label = mylab), fontface = "bold", color = "black") +
  coord_cartesian(xlim = c(0, 1), 
                  ylim = c(1, 102),
                  clip = "off") +
  theme_void()

print(ann)

p2 <- p + ann  + 
  plot_layout(widths = c(5, 1))

print(p2)
ggsave("output/z-scored/normalised-performance-score.pdf", p2, units = "in", height = 16, width = 16)
