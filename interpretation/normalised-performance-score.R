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
    tibble::column_to_rownames(var = "problem") |>
    drop_na()

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
#' @param model_type \code{character} denoting the type of model that fit and whose results should be loaded
#' @return \code{ggplot} containing the NPS plot
#' @author Trent Henderson
#' 

nps <- function(model_type = c("svm", "xgboost", "pyridge", "glmnet")){
  
  model_type <- match.arg(model_type)
  stopifnot(model_type %in% c("svm", "xgboost", "pyridge", "glmnet"))
  
  # Read in data
  
  files <- list.files(paste0("classification-models/results/", model_type, "/"))
  accuracies <- vector(mode = "list", length = length(files))
  
  for(i in files){
    feature_sets <- read.csv(paste0("classification-models/results/", model_type, "/", i))
    accuracies[[match(i, files)]] <- feature_sets
  }
  
  accuracies <- do.call("rbind", accuracies) |>
    filter(feature_set != "timegp") |> # From another related project which used the same methodology
    filter(problem != "Fungi")
  
  #---------------------
  # Compute global stats
  #---------------------
  
  # Calculate using only feature sets
  
  benchmarks <- accuracies |>
    filter(feature_set %in% c("catch22", "feasts", "tsfeatures", "Kats", "TSFEL", "tsfresh")) |>
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
  
  # Mean NPS by set
  
  nps_sets <- z_scores |>
    reframe(.mean = mean(z, na.rm = TRUE), .by = "feature_set") |>
    arrange(desc(.mean))
  
  #---------------------------------
  # Generate hierarchical clustering
  #---------------------------------
  
  baseline_z <- z_scores |>
    filter(feature_set %in% c("quantiles", "FFT coef. (Mag^2 + Angle)", "FFT (Mag^2 + Angle) + quantiles")) |>
    filter(z > 1) |>
    group_by(problem) |>
    mutate(the_rank = dense_rank(-z)) |>
    ungroup() |>
    filter(the_rank == 1) |>
    dplyr::select(-c(the_rank))
  
  uniques <- unique(baseline_z$feature_set)
  baseline_storage <- vector(mode = "list", length = length(uniques))
  
  for(i in uniques){
    tmp_probs <- baseline_z |>
      filter(feature_set == i) |>
      pull(problem)
    
    tmp_cluster <- cluster_problems(z_scores, tmp_probs, z_scores, benchmarks_sets)
    baseline_storage[[match(i, uniques)]] <- tmp_cluster
  }
  
  baseline_storage <- do.call("rbind", baseline_storage)

  # Cluster 4: Problems where baseline < mean

  cluster_4 <- z_scores |>
    filter(problem %ni% baseline_z$problem) |>
    dplyr::select(c(problem)) |>
    distinct() |>
    pull(problem)

  cluster_4 <- cluster_problems(z_scores, cluster_4, z_scores, benchmarks_sets)

  # Order baseline problems by which baseline set won, then by within-group
  # clustering (mirrors how baseline_storage was constructed)

  baseline_problem_order <- unique(baseline_storage$problem)

  # Dynamic plot boundaries

  n4 <- length(unique(cluster_4$problem))
  n_total <- n4 + length(baseline_problem_order)
  boundary <- n4 + 0.5

  # Bind together

  clusters <- bind_rows(baseline_storage, cluster_4)
  
  #----------
  # Draw plot
  #----------

  # Bold benchmark feature sets on the x-axis (order matches nps_sets$feature_set)

  x_faces <- ifelse(nps_sets$feature_set %in% c("catch22", "feasts", "tsfeatures", "Kats", "TSFEL", "tsfresh"),
                    "bold", "plain")

  # Fill gradient. SVM scores extend higher than XGBoost, so the upper bound goes to NPS = 2.5
  
  if(model_type == "pyridge"){
    fill_scale <- scale_fill_gradientn(colours = c("#0571B0", "#92C5DE", "white", "white", "white", "#F4A582", "#CA0020"),
                                       values = scales::rescale(c(-5, -3, -1, 0, 1, 2, 3), to = c(0, 1), from = c(-5, 3)),
                                       breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3),
                                       labels = c("≤-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3"),
                                       limits = c(-5, 3))
  } else if (model_type == "svm"){
    fill_scale <- scale_fill_gradientn(colours = c("#0571B0", "#92C5DE", "white", "white", "white", "#F4A582", "#CA0020"),
                                       values = c(0, 1/5.5, 2/5.5, 3/5.5, 4/5.5, 4.5/5.5, 1),
                                       breaks = c(-3, -2.5, -2, -1, -0.5, 0, 0.5, 1, 2, 2.5),
                                       labels = c("≤-3", "-2.5", "-2", "-1", "-0.5", "0", "0.5", "1", "2", "2.5"),
                                       limits = c(-3, 2.5))
  } else{
    fill_scale <- scale_fill_gradientn(colours = c("#0571B0", "#92C5DE", "white", "white", "white", "#F4A582", "#CA0020"),
                                       values = c(0, 1/5, 2/5, 3/5, 4/5, 4.5/5, 1),
                                       breaks = c(-3, -2.5, -2, -1, -0.5, 0, 0.5, 1, 2),
                                       labels = c("≤-3", "-2.5", "-2", "-1", "-0.5", "0", "0.5", "1", "2"),
                                       limits = c(-3, 2))
  }

  p <- clusters |>
    mutate(value = ifelse(value < -5, -5, value),
           value = ifelse(value > 5, 5, value)) |> # For visual clarity
    mutate(problem = factor(problem, levels = c(as.character(rev(unique(cluster_4$problem))),
                                                as.character(baseline_problem_order)),
                            ordered = TRUE)) |>
    mutate(feature_set = factor(feature_set, levels = nps_sets$feature_set)) |>
    ggplot(aes(x = feature_set, y = problem, fill = value)) +
    geom_tile() +
    
    # Annotate regions
    
    geom_rect(aes(xmin = 0.5, xmax = 9.5, ymin = boundary, ymax = n_total + 0.5), fill = NA, colour = "black", linewidth = 1) + # baseline > 1
    geom_rect(aes(xmin = 0.5, xmax = 9.5, ymin = 0.5, ymax = boundary), fill = NA, colour = "black", linewidth = 1) + # baseline < 1
    geom_vline(aes(xintercept = 6.5), linetype = "dashed", colour = "black", linewidth =  0.6) +
    
    # Labels and colourmap formatting
    
    labs(x = "Feature set",
         y = "Problem",
         fill = "Normalized performance score") +
    fill_scale +
    
    # Clean up feature set names for visual clarity
    
    scale_x_discrete(labels = function(x) {
      x <- gsub("Mag^2", "Mag²", x, fixed = TRUE)
      gsub(" + quantiles", "\n+ quantiles", x, fixed = TRUE)
    }) +
    
    # Format plot
    
    theme_bw() +
    coord_cartesian(xlim = c(1, 9), clip = "off") +
    theme(legend.position = "bottom",
          legend.key.width = unit(2, "cm"),
          panel.grid = element_blank(),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 11),
          panel.border = element_blank(),
          axis.text.x = element_text(angle = 90, size = 11, face = x_faces))
  
  # Add side annotations for A and B
  
  label_data <- data.frame(x = rep(0.1, times = 2),
                           y = c(125, n4 - 2),
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

p_pyridge <- nps("pyridge")
print(p_pyridge)
ggsave("output/normalised-performance-score-pyridge.pdf", p_pyridge, units = "in", height = 19, width = 15)
