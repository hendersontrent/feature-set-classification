#------------------------------------------
# This script sets out to produce analysis
# of feature set performance against bake-off
# paper benchmarks
#
# NOTE: This script requires setup.R and
# analysis/compute-features.R and
# analysis/fit-classifiers.R to have been 
# run first
#-----------------------------------------

#-------------------------------------
# Author: Trent Henderson, 5 July 2022
#-------------------------------------

# Grab benchmark results

benchmarks <- pull_benchmark_results() %>%
  dplyr::select(c(problem, method, accuracy))

#---------------- Compute best feature set ----------------

 # Load classification results for feature sets and compute winner

load("data/outputs.Rda")
load("data/outputs_aggregate.Rda")

# Filter to just problems in calculated sets

benchmarks <- benchmarks %>%
  filter(problem %in% unique(outputs$problem))

#' Find best set for each problem
#' @param outputs_data \code{data.frame} of individual feature set classification results
#' @param outputs_agg_data \code{data.frame} of all features classification results
#' @param benchmark_data \code{data.frame} of benchmark algorithm results
#' @return object of class \code{list}
#' @author Trent Henderson
#' 

find_feature_winners <- function(outputs_data, outputs_agg_data, benchmark_data){
  
  outputs_data <- outputs_data %>%
    mutate(method = case_when(
      method == "tsfel" ~ "TSFEL",
      method == "kats"  ~ "Kats",
      TRUE              ~ method)) %>%
    dplyr::select(c(problem, method, accuracy))
  
  #---------------------- Calculate p-values -----------------------
  
  # Set up single dataframe
  
  reduced <- outputs_data %>%
    dplyr::select(c(problem, method, accuracy))
  
  reduced2 <- outputs_agg_data %>%
    mutate(method = "All features") %>%
    dplyr::select(c(problem, method, accuracy))
  
  all_data <- bind_rows(reduced, reduced2, benchmark_data)
  
  # Iterate over every pairwise comparison to all features for each problem
  
  combns <- crossing(unique(all_data$method), unique(all_data$method), unique(all_data$problem), .name_repair = "unique") %>%
    rename(set1 = 1, 
           set2 = 2,
           problem = 3)
  
  combns <- combns[!duplicated(data.frame(t(apply(combns, 1, sort)))), ] # Remove duplicates since we get both set's values in the function
  combns <- combns[combns$set1 != combns$set2, ]
  
  comps <- 1:nrow(combns) %>%
    purrr::map_df(~ calculate_p_values_acc(data = all_data, combn_data = combns, rownum = .x))
  
  # Add in accuracy values to get direction
  
  single_accs <- outputs_data %>%
    group_by(problem, method) %>%
    summarise(accuracy_set = mean(accuracy, na.rm = TRUE)) %>%
    ungroup()
  
  all_accs <- outputs_agg_data %>%
    group_by(problem) %>%
    summarise(accuracy_all = mean(accuracy, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(method = "All features")
  
  bench_accs <- benchmark_data %>%
    group_by(problem, method) %>%
    summarise(accuracy_bench = mean(accuracy, na.rm = TRUE)) %>%
    ungroup()
  
  sets <- unique(outputs_data$method)
  benches <- unique(benchmark_data$method)
  
  comps2 <- comps %>%
    left_join(single_accs, by = c("problem" = "problem", "set1" = "method")) %>%
    left_join(all_accs, by = c("problem" = "problem", "set1" = "method")) %>%
    left_join(bench_accs, by = c("problem" = "problem", "set1" = "method")) %>%
    left_join(single_accs, by = c("problem" = "problem", "set2" = "method")) %>%
    left_join(all_accs, by = c("problem" = "problem", "set2" = "method")) %>%
    left_join(bench_accs, by = c("problem" = "problem", "set2" = "method")) %>%
    mutate(set1_accuracy = case_when(
            set1 %in% sets         ~ accuracy_set.x,
            set1 %in% benches      ~ accuracy_bench.x,
            set1 == "All features" ~ accuracy_all.x)) %>%
    mutate(set2_accuracy = case_when(
      set2 %in% sets         ~ accuracy_set.y,
      set2 %in% benches      ~ accuracy_bench.y,
      set2 == "All features" ~ accuracy_all.y)) %>%
    dplyr::select(c(problem, set1, set2, set1_accuracy, set2_accuracy, t_statistic, p_value))

  # Find best "features" for each problem
  
  comps_feats <- comps2 %>%
    mutate(flag = ifelse(set1 %in% append(sets, "All features") & set2 %in% append(sets, "All features"), TRUE, FALSE)) %>%
    filter(flag) %>%
    dplyr::select(-c(flag)) %>%
    mutate(winning_accuracy = case_when(
      set1_accuracy > set2_accuracy  ~ set1_accuracy,
      set2_accuracy > set1_accuracy  ~ set2_accuracy)) %>%
    group_by(problem) %>%
    slice_max(order_by = winning_accuracy, n = 1) %>%
    ungroup() %>%
    mutate(winner = ifelse(set1_accuracy == winning_accuracy, set1, set2)) %>%
    dplyr::select(c(problem, winner_set)) %>%
    distinct()
  
  
  #----------------------------------------
  # Find best "benchmarks" for each problem
  #----------------------------------------
  
  # Overall results
  
  comps_benches <- comps2 %>%
    mutate(flag = ifelse(set1 %in% benches & set2 %in% benches, TRUE, FALSE)) %>%
    filter(flag) %>%
    dplyr::select(-c(flag)) %>%
    mutate(winning_accuracy = case_when(
      set1_accuracy > set2_accuracy  ~ set1_accuracy,
      set2_accuracy > set1_accuracy  ~ set2_accuracy,
      set1_accuracy == set2_accuracy ~ 999)) %>%
    mutate(winning_accuracy = ifelse(winning_accuracy == 999, NA, winning_accuracy))
  
  # Find problems where a tie exists
  
  comps_benches_tie_list <- comps_benches %>%
    mutate(flag = ifelse(is.na(winning_accuracy), TRUE, FALSE)) %>%
    group_by(problem, flag) %>%
    summarise(counter = n()) %>%
    group_by(problem) %>%
    summarise(counter = n()) %>%
    ungroup()
  
  ties_probs <- comps_benches_tie_list %>%
    filter(counter == 2) %>%
    dplyr::select(c(problem)) %>%
    pull()
  
  no_ties_probs <- comps_benches_tie_list %>%
    filter(counter == 1) %>%
    dplyr::select(c(problem)) %>%
    pull()
  
  # Handle majority cases with no ties
  
  comps_benches_no_ties <- comps_benches %>%
    filter(!is.na(winning_accuracy))
  
  # Handle minority cases with no ties
  
  comps_benches_ties <- comps_benches %>%
    filter(is.na(winning_accuracy))
  
    group_by(problem) %>%
    slice_max(order_by = winning_accuracy, n = 1) %>%
    ungroup() %>%
    mutate(winner = ifelse(set1_accuracy == winning_accuracy, set1, set2)) %>%
    dplyr::select(c(problem, winner_bench)) %>%
    distinct()
  
  winners <- comps_feats %>%
    inner_join(comps_benches, by = c("problem" = "problem")) %>%
    mutate(flag = TRUE)
  
  # Filter overall results by "best of" lists and determine significance
  
  comps3 <- comps2 %>%
    left_join(winners, by = c("problem" = "problem", "set1" = "winner_set", "set2" = "winner_bench")) %>%
    left_join(winners, by = c("problem" = "problem", "set1" = "winner_bench", "set2" = "winner_set")) %>%
    filter(flag) %>%
    dplyr::select(-c(flag)) %>%
    mutate(flag = case_when(
      is.na(p_value)                                  ~ "Zero variance for one/more sets",
      p_value > .05                                   ~ "Non-Significant difference",
      p_value < .05 & set1_accuracy > set2_accuracy   ~ set1,
      p_value < .05 & set1_accuracy < set2_accuracy   ~ set2))

  return(comps)
}

winners <- find_feature_winners(outputs_data = outputs, outputs_agg_data = outputs_aggregate)

#--------------------- Draw plots ------------------

# Create palette for whoever is top performer

mypal <- c("All Features" = "grey50",
           "TS-CHIEF" = "black",
           "HIVE-COTE v1.0" = "black",
           "ROCKET" = "black",
           "InceptionTime" = "black",
           "STC" = "black",
           "ResNet" = "black",
           "ProximityForest" = "black",
           "WEASEL" = "black",
           "S-BOSS" = "black",
           "cBOSS" = "black",
           "BOSS" = "black",
           "RISE" = "black",
           "TSF" = "black",
           "catch22" = "#1B9E77",
           "feasts" = "#D95F02",
           "Kats" = "#7570B3",
           "tsfeatures" = "#E7298A",
           "TSFEL" = "#66A61E",
           "tsfresh" = "#E6AB02")

myshapes <- c("All Features" = 16,
              "TS-CHIEF" = 0,
              "HIVE-COTE v1.0" = 1,
              "ROCKET" = 2,
              "InceptionTime" = 3,
              "STC" = 4,
              "ResNet" = 5,
              "ProximityForest" = 6,
              "WEASEL" = 7,
              "S-BOSS" = 8,
              "cBOSS" = 9,
              "BOSS" = 10,
              "RISE" = 11,
              "TSF" = 12,
              "catch22" = 16,
              "feasts" = 16,
              "Kats" = 16,
              "tsfeatures" = 16,
              "TSFEL" = 16,
              "tsfresh" = 16)

# Define coordinates for upper triangle to shade

upper_tri <- data.frame(x = c(0, 0, 100), y = c(0, 100, 100))

# Draw scatterplot

p <- both %>%
  ggplot(aes(x = worst_balanced_accuracy_mean, y = best_balanced_accuracy_mean)) +
  geom_polygon(data = upper_tri, aes(x = x, y = y), fill = "steelblue2", alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, colour = "grey50", lty = "dashed") +
  geom_errorbar(aes(ymin = lower_y, ymax = upper_y, colour = top_performer)) +
  geom_errorbarh(aes(xmin = lower_x, xmax = upper_x, colour = top_performer)) +
  geom_point(aes(colour = top_performer), size = 2) +
  annotate("text", x = 80, y = 10, label = "Time-series features better") +
  annotate("text", x = 20, y = 90, label = "Leading benchmark better") +
  labs(title = "Comparison of feature sets versus benchmark algorithms across UCR/UEA repository univariate problems",
       subtitle = "Plots a subset of 53 problems where preliminary analysis showed mean and variance did not outperform chance",
       x = "Classification accuracy time-series features (%)",
       y = "Classification accuracy benchmark algorithm (%)",
       colour = NULL,
       shape = NULL) +
  scale_x_continuous(labels = function(x)paste0(x, "%")) + 
  scale_y_continuous(labels = function(x)paste0(x, "%")) + 
  scale_colour_manual(values = mypal,
                      name = "Algorithm and Set") +
  scale_shape_manual(values = myshapes,
                     name = "Algorithm and Set") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank())

print(p)
ggsave("output/non-z-scored/features-vs-bench.pdf", p)
