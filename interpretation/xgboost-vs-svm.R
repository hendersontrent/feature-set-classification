#-------------------------------------
# This script compares the performance
# by feature set between a linear SVM
# and an RBF SVM
#-------------------------------------

#-------------------------------------
# Author: Trent Henderson, 21 May 2026
#-------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggdist)
library(scales)
library(Hmisc)
library(correctR)

#--------------- Define functions ---------------

#' Get train-test sample sizes for each problem
#' 
#' @return \code{data.frame} of problem summaries
#' @author Trent Henderson
#' 

get_n <- function(){
  
  n_storage <- vector(mode = "list", length = length(list.files("feature-calculations/train-test-labels/")))
  
  for(i in 1:length(n_storage)){
    rm(label)
    load(paste0("feature-calculations/train-test-labels/", list.files("feature-calculations/train-test-labels/")[i]))
    
    n_tmp <- label |>
      distinct() |>
      reframe(counter = n(), .by = c("problem", "train_test")) |>
      pivot_wider(id_cols = "problem", names_from = "train_test", values_from = "counter")
    
    n_storage[[i]] <- n_tmp
  }
  
  n_storage <- do.call("rbind", n_storage)
  return(n_storage)
}

#' Pull and reshape classification results
#' 
#' @return \code{data.frame} containing the results
#' @author Trent Henderson
#' 

linear_xg <- function(){
  
  # Pull linear SVM results
  
  files <- list.files(paste0("classification-models/results/svm"))
  accuracies <- vector(mode = "list", length = length(files))
  
  for(i in files){
    feature_sets <- read.csv(paste0("classification-models/results/svm/", i))
    accuracies[[match(i, files)]] <- feature_sets
  }
  
  accuracies <- do.call("rbind", accuracies) |>
    filter(feature_set != "timegp") |> # From another related project which used the same methodology
    rename(accuracy_linear = accuracy) |>
    dplyr::select(-c(classifier))
  
  # Pull RBF SVM results
  
  files2 <- list.files(paste0("classification-models/results/xgboost"))
  accuracies2 <- vector(mode = "list", length = length(files2))
  
  for(j in files2){
    feature_sets2 <- read.csv(paste0("classification-models/results/xgboost/", j))
    accuracies2[[match(j, files2)]] <- feature_sets2
  }
  
  accuracies2 <- do.call("rbind", accuracies2) |>
    rename(accuracy_xg = accuracy) |>
    dplyr::select(-c(classifier))
  
  # Merge and pivot
  
  accuracies3 <- accuracies |>
    inner_join(accuracies2) |>
    mutate(.diff = accuracy_xg - accuracy_linear)
  
  return(accuracies3)
}

# Run function

results <- linear_xg()
  
#--------------- Do analysis ---------------

# Compute average change due to XGBoost (i.e., a more complex, nonlinear classifier)

results |>
  reframe(.mean = mean(.diff))

results |>
  reframe(.mean = mean(.diff), .by = "feature_set") |>
  arrange(-.mean)

# Visualise as distributional change by feature set

pals <- palette.colors(10, "Tableau 10")

mypal <- c("catch22" = pals[8],
           "feasts" = pals[7],
           "Kats" = pals[6],
           "tsfeatures" = pals[5],
           "TSFEL" = pals[4],
           "tsfresh" = pals[3])

p_dist <- results |>
  #filter(.diff >= -0.5) |>
  ggplot(aes(x = .diff, y = feature_set)) +
  stat_halfeye(
    aes(
      fill = feature_set,
      fill_ramp = after_stat(ifelse(x <= 0, 0, 1))
    )
  ) +
  stat_dotsinterval(
    aes(
      fill = feature_set,
      fill_ramp = after_stat(ifelse(x <= 0, 0, 1))
      ),
    side = "bottom", scale = 0.7, slab_linewidth = NA) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "black", linewidth = 0.8) +
  annotate("text", x = 0.20, y = 6.75, label = "XGBoost better", fontface = "bold") +
  annotate("text", x = -0.20, y = 6.75, label = "Linear SVM better", fontface = "bold") +
  scale_fill_manual(values = mypal, guide = "none") +
  scale_colour_manual(values = mypal, guide = "none") +
  scale_fill_ramp_continuous(from = "grey40", guide = "none") +
  labs(
    x = "Difference in absolute classification accuracy\n(XGBoost - Linear SVM)",
    y = "Feature set"
  ) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

print(p_dist)
ggsave("output/xgboost-vs-svm-halfeye.pdf", p_dist, width = 10, height = 10)

# Visualise as population pyramid by feature set and problem

long_results <- results |>
  pivot_longer(
    cols = c(accuracy_linear, accuracy_xg),
    names_to = "classifier",
    values_to = "accuracy"
  ) |>
  mutate(classifier = ifelse(classifier == "accuracy_linear", "Linear SVM", "XGBoost"))

ci_data <- long_results |>
  group_by(problem, feature_set, classifier) |>
  summarise(
    smean = Hmisc::smean.cl.normal(accuracy)[["Mean"]],
    lower = Hmisc::smean.cl.normal(accuracy)[["Lower"]],
    upper = Hmisc::smean.cl.normal(accuracy)[["Upper"]],
    .groups = "drop"
  ) |>
  group_by(problem, feature_set) |>
  mutate(winner = classifier[which.max(smean)]) |>
  ungroup() |>
  mutate(
    status = ifelse(classifier == winner,
                    ifelse(classifier == "Linear SVM", "Linear SVM (best)", "XGBoost (best)"),
                    "Loser"),
    bar_val   = ifelse(classifier == "Linear SVM", -smean, smean),
    lower_bar = ifelse(classifier == "Linear SVM", -upper, lower),
    upper_bar = ifelse(classifier == "Linear SVM", -lower, upper)
  )

problem_order <- ci_data |>
  filter(classifier == "XGBoost") |>
  summarise(avg = mean(smean), .by = "problem") |>
  arrange(avg) |>
  pull(problem)

ci_data <- ci_data |>
  mutate(problem = factor(problem, levels = problem_order))

acc_max <- max(abs(ci_data$bar_val), na.rm = TRUE)
acc_breaks <- seq(-acc_max, acc_max, length.out = 5)

p <- ggplot(ci_data, aes(x = bar_val, y = problem, fill = status)) +
  geom_col() +
  geom_errorbarh(aes(xmin = lower_bar, xmax = upper_bar), height = 0.2, colour = "black") +
  labs(x = "Mean classification accuracy (%)",
       y = "Problem") +
  scale_x_continuous(
    breaks = acc_breaks,
    labels = function(x) scales::percent(abs(x), accuracy = 1)
  ) +
  scale_fill_manual(
    values = c(
      "Linear SVM (best)" = "#D95F02FF",
      "XGBoost (best)" = "#1B9E77FF",
      "Loser" = "grey80"
    ),
    name = "Better classifier"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        panel.grid = element_blank()) +
  facet_wrap(~feature_set)

print(p)
ggsave("output/xgboost-vs-svm-pyramid.pdf", p, width = 16, height = 28)
