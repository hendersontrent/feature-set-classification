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
load("data/outputs_z_bp.Rda")

#---------------------- Calculate scores--------------------

#------------------
# Overall benchmark
#------------------

# Calculate using only feature sets

benchmarks <- outputs_z %>%
  group_by(problem) %>%
  summarise(overall_avg = mean(accuracy, na.rm = TRUE),
            stddev = sd(accuracy, na.rm = TRUE)) %>%
  ungroup()

# Bind both datasets together

outputs_z <- bind_rows(outputs_z, outputs_z_bp)

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

#' Hierarchical cluster on rows (problems)
#' 
#' @param data \code{data.frame} containing NPS results
#' @param problem_vector \code{character} vector denoting problems to filter to
#' @return \code{data.frame} of clustered data
#' @author Trent Henderson
#' 

cluster_problems <- function(data, problem_vector){
  
  z_scores_mat <- z_scores %>%
    filter(problem %in% problem_vector) %>%
    dplyr::select(c(problem, method, z)) %>%
    pivot_wider(id_cols = "problem", names_from = "method", values_from = "z") %>%
    tibble::column_to_rownames(var = "problem")
  
  row.order <- stats::hclust(stats::dist(z_scores_mat, method = "euclidean"), method = "average")$order
  z_scores_mat <- z_scores_mat[row.order, ]
  
  z_scores_mat <- reshape2::melt(as.matrix(z_scores_mat)) %>%
    rename(problem = Var1,
           method = Var2) %>%
    inner_join(benchmarks_sets, by = c("method" = "method"))
  
  return(z_scores_mat)
}

#-------------------------------------
# Cluster 1: Problems where FFT > mean
#-------------------------------------

cluster_1 <- z_scores %>%
  filter(method == "fft") %>%
  filter(z > 1) %>%
  filter(problem != "EthanolLevel") %>% # Quantiles does better so put it in Cluster 2
  pull(problem)

cluster_1 <- cluster_problems(z_scores, cluster_1)

#-------------------------------------------
# Cluster 2: Problems where quantiles > mean
#-------------------------------------------

cluster_2 <- z_scores %>%
  filter(method == "quantiles") %>%
  filter(z > 1) %>%
  pull(problem)

cluster_2 <- cluster_problems(z_scores, cluster_2)

#----------------------------------------------------------
# Cluster 3: All other problems (core focus of the graphic)
#----------------------------------------------------------

cluster_3 <- z_scores %>%
  filter(problem %ni% append(cluster_1$problem, cluster_2$problem)) %>%
  pull(problem)

cluster_3 <- cluster_problems(z_scores, cluster_3)

#--------------
# Bind together
#--------------

clusters <- bind_rows(cluster_3, cluster_2, cluster_1)

#---------------------- Draw graphic -----------------------

# Main plot

p <- clusters %>%
  mutate(value = ifelse(value < -3.5, -3.5, value)) %>% # For visual clarity
  mutate(problem = factor(problem, levels = append(append(as.character(rev(unique(cluster_3$problem))),
                                                   as.character(rev(unique(cluster_2$problem)))),
                                                   as.character(rev(unique(cluster_1$problem)))), 
                          ordered = TRUE)) %>%
  ggplot(aes(x = reorder(method, -global_avg), y = problem, fill = value)) +
  geom_tile() +
  geom_rect(aes(xmin = 6.5, xmax = 7.5, ymin = 95.5, ymax = 102.5), fill = NA, colour = "black", size = 1) +
  geom_rect(aes(xmin = 7.5, xmax = 8.5, ymin = 92.5, ymax = 99.5), fill = NA, colour = "black", size = 1) +
  geom_rect(aes(xmin = 0.5, xmax = 6.5, ymin = 0.5, ymax = 92.5), fill = NA, colour = "black", size = 1) +
  labs(x = "Feature set",
       y = "Problem",
       fill = "Normalised performance score") +
  scale_fill_gradientn(colours = c("#0571B0", "#92C5DE", "white", "white", "white", "#F4A582", "#CA0020"),
                       breaks = c(-3.5, -3, -2.5, -2, -1, -0.5, 0, 0.5, 1, 2, 2.5),
                       labels = c("< -3.5", "-3", "-2.5", "-2", "-1", "-0.5", "0", "0.5", "1", "2", "2.5"),
                       limits = c(-3.5, 2.5)) +
  theme_bw() +
  coord_cartesian(xlim = c(1, 8), clip = "off") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        panel.grid = element_blank(),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11))

# Side annotations

label_data <- data.frame(x = rep(0.5, times = 3),
                         y = c(104, 100, 50),
                         mylab = c("FFT > mean", "Quantiles > mean", "Meaningful feature\nset differences"))

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
