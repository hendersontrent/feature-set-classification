#---------------------------------------
# This script calculates the normalised
# performance score across the database
# of problems
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 4 May 2026
#---------------------------------------

library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(patchwork)

"%ni%" <- Negate("%in%")

#--------------- Define functions ---------------

#' Hierarchical cluster on rows (problems)
#' 
#' @param data \code{data.frame} containing NPS results
#' @param problem_vector \code{character} vector denoting problems to filter to
#' @param z \code{data.frame} of z-scores
#' @param b \code{data.frame} of benchmark results
#' @return \code{data.frame} of clustered data
#' @author Trent Henderson
#' 

cluster_problems <- function(data, problem_vector, z, b){

  z_scores_mat <- data |>
    filter(problem %in% problem_vector) |>
    dplyr::select(c(problem, feature_set, z)) |>
    pivot_wider(id_cols = "problem", names_from = "feature_set", values_from = "z") |>
    tibble::column_to_rownames(var = "problem")

  if(nrow(z_scores_mat) >= 2){
    row.order <- stats::hclust(stats::dist(z_scores_mat, method = "euclidean"), method = "average")$order
    z_scores_mat <- z_scores_mat[row.order, ]
  }

  z_scores_mat <- reshape2::melt(as.matrix(z_scores_mat)) |>
    rename(problem = Var1,
           feature_set = Var2) |>
    inner_join(b, by = c("feature_set" = "feature_set")) |>
    mutate(problem = as.character(problem))

  return(z_scores_mat)
}

#' Compute and visualise the normalised performance score
#' 
#' @param model_type \code{character} denoting the type of model that fit and whose results should be loaded. Can be one of \code{"glmnet"}, \code{"svm"}, or \code{"xgboost"}
#' @return \code{ggplot} containing the NPS plot
#' @author Trent Henderson
#' 

nps <- function(model_type = c("glmnet", "svm", "xgboost")){
  
  model_type <- match.arg(model_type)
  stopifnot(model_type %in% c("glmnet", "svm", "xgboost"))
  
  # Read in data
  
  files <- list.files(paste0("classification-models/results/", model_type))
  accuracies <- vector(mode = "list", length = length(files))
  
  for(i in files){
    feature_sets <- read.csv(paste0("classification-models/results/", model_type, "/", i))
    baseline_sets <- read.csv(paste0("classification-models/results-baseline/", model_type, "/", i))
    all_sets <- bind_rows(feature_sets, baseline_sets)
    accuracies[[match(i, files)]] <- all_sets
  }
  
  accuracies <- do.call("rbind", accuracies) |>
    filter(feature_set != "timegp") |> # From another related project which used the same methodology
    filter(problem != "Fungi")
  
  #---------------------
  # Compute global stats
  #---------------------
  
  # Calculate using only feature sets
  
  benchmarks <- accuracies |>
    reframe(.mean = mean(accuracy, na.rm = TRUE),
            .sd = sd(accuracy, na.rm = TRUE),
            .by = "problem")
  
  #---------------------
  # Calculate NPS values
  #---------------------
  
  # Calculate z-scores
  
  z_scores <- accuracies |>
    reframe(x = mean(accuracy, na.rm = TRUE), .by = c("problem", "feature_set")) |>
    inner_join(benchmarks, by = c("problem" = "problem")) |>
    group_by(problem, feature_set) |>
    mutate(z = (x - .mean) / .sd) |>
    ungroup()
  
  # Mean accuracy by set
  
  benchmarks_sets <- accuracies |>
    reframe(.mean = mean(accuracy, na.rm = TRUE), .by = "feature_set")
  
  #---------------------------------
  # Generate hierarchical clustering
  #---------------------------------
  
  # top_set <- accuracies |>
  #   reframe(.mean = mean(accuracy), .by = c("problem", "feature_set")) |>
  #   group_by(problem) |>
  #   slice_max(.mean) |>
  #   ungroup() |>
  #   filter(feature_set %in% c("FFT coefficients", "quantiles", "FFT + quantiles"))
  
  baseline_z <- z_scores |>
    filter(feature_set %in% c("FFT coefficients", "quantiles", "FFT + quantiles")) |>
    filter(z > 1) |>
    #filter(problem %in% unique(top_set$problem)) |>
    group_by(problem) |>
    mutate(the_rank = dense_rank(-z)) |>
    ungroup() |>
    filter(the_rank == 1) |>
    dplyr::select(-c(the_rank))
  
  # Problem vectors for each cluster

  cluster_1_probs <- baseline_z |>
    filter(feature_set == "quantiles") |>
    pull(problem)

  cluster_2_probs <- baseline_z |>
    filter(feature_set == "FFT + quantiles") |>
    pull(problem)

  cluster_3_probs <- baseline_z |>
    filter(feature_set == "FFT coefficients") |>
    pull(problem)

  # Data frames with z-scores for each cluster

  cluster_1 <- cluster_problems(z_scores, cluster_1_probs, z_scores, benchmarks_sets)
  cluster_2 <- cluster_problems(z_scores, cluster_2_probs, z_scores, benchmarks_sets)
  cluster_3 <- cluster_problems(z_scores, cluster_3_probs, z_scores, benchmarks_sets)

  # Cluster 4: Problems where baseline < mean

  cluster_4 <- z_scores |>
    filter(problem %ni% baseline_z$problem) |>
    pull(problem)

  cluster_4 <- cluster_problems(z_scores, cluster_4, z_scores, benchmarks_sets)

  # Joint hclust on all baseline > mean problems

  baseline_order <- cluster_problems(z_scores, unique(baseline_z$problem), z_scores, benchmarks_sets)
  joint_order <- rev(unique(baseline_order$problem))

  cluster_3_levels <- joint_order[joint_order %in% cluster_3_probs]
  cluster_2_levels <- joint_order[joint_order %in% cluster_2_probs]
  cluster_1_levels <- joint_order[joint_order %in% cluster_1_probs]

  # Dynamic plot boundaries

  n4 <- length(unique(cluster_4$problem))
  n_total <- n4 + length(joint_order)
  boundary <- n4 + 0.5

  # Bind together

  clusters <- bind_rows(cluster_1, cluster_2, cluster_3, cluster_4)
  
  #----------
  # Draw plot
  #----------
  
  p <- clusters |>
    #mutate(value = ifelse(value < -3.5, -3.5, value)) |> # For visual clarity
    mutate(problem = factor(problem, levels = c(as.character(rev(unique(cluster_4$problem))),
                                                as.character(cluster_3_levels),
                                                as.character(cluster_2_levels),
                                                as.character(cluster_1_levels)),
                            ordered = TRUE)) |>
    mutate(feature_set = factor(feature_set, levels = c("tsfresh", "tsfeatures", "TSFEL",
                                                        "feasts", "Kats", "catch22",
                                                        "FFT + quantiles", "FFT coefficients", "quantiles"))) |>
    ggplot(aes(x = feature_set, y = problem, fill = value)) +
    geom_tile() +
    geom_rect(aes(xmin = 0.5, xmax = 9.5, ymin = boundary, ymax = n_total + 0.5), fill = NA, colour = "black", linewidth = 1) + # baseline > mean
    geom_rect(aes(xmin = 0.5, xmax = 9.5, ymin = 0.5, ymax = boundary), fill = NA, colour = "black", linewidth = 1) + # baseline < mean
    labs(x = "Feature set",
         y = "Problem",
         fill = "Normalized performance score") +
    scale_fill_gradientn(colours = c("#0571B0", "#92C5DE", "white", "white", "white", "#F4A582", "#CA0020"),
                         values = c(0, 1/5.5, 2/5.5, 3/5.5, 4/5.5, 4.5/5.5, 1),
                         breaks = c(-3, -2.5, -2, -1, -0.5, 0, 0.5, 1, 2, 2.5),
                         labels = c("-3", "-2.5", "-2", "-1", "-0.5", "0", "0.5", "1", "2", "2.5"),
                         limits = c(-3, 2.5)) +
    theme_bw() +
    coord_cartesian(xlim = c(1, 9), clip = "off") +
    theme(legend.position = "bottom",
          legend.key.width = unit(2, "cm"),
          panel.grid = element_blank(),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 11),
          panel.border = element_blank())
  
  # Side annotations
  
  label_data <- data.frame(x = rep(0.1, times = 2),
                           y = c(127, n4 - 2),
                           mylab = c("A", "B"))

  ann <- ggplot(data = label_data) +
    geom_text(aes(x = x, y = y, label = mylab), fontface = "bold", color = "black", size = 8) +
    coord_cartesian(xlim = c(0, 1),
                    ylim = c(1, n_total),
                    clip = "off") +
    theme_void()
  
  p1 <- p + ann + 
    plot_layout(widths = c(5, 1))
  
  return(p1)
}

#--------------- Generate plot ---------------

p_svm <- nps("svm")
print(p_svm)
p_xg <- nps("xgboost")
print(p_xg)
ggsave("output/normalised-performance-score.pdf", p_svm, units = "in", height = 19, width = 15)
ggsave("output/normalised-performance-score-xgboost.pdf", p_xg, units = "in", height = 19, width = 15)
