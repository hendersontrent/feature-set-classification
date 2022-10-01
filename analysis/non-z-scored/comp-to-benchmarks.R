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

find_winners <- function(outputs_data, outputs_agg_data, benchmark_data){
  
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
    purrr::map_df(~ calculate_p_values_acc(data = all_data, combn_data = combns, rownum = .x)) %>%
    mutate(p_value_adj = p.adjust(p_value, method = "holm"))
  
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
    dplyr::select(c(problem, set1, set2, set1_accuracy, set2_accuracy, t_statistic, p_value, p_value_adj))

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
    mutate(winner_set = ifelse(set1_accuracy == winning_accuracy, set1, set2)) %>%
    dplyr::select(c(problem, winner_set)) %>%
    distinct()
  
  # Find best "benchmarks" for each problem
  
  comps_benches <- comps2 %>%
    mutate(flag = ifelse(set1 %in% benches & set2 %in% benches, TRUE, FALSE)) %>%
    filter(flag) %>%
    dplyr::select(-c(flag)) %>%
    mutate(winning_accuracy = case_when(
      set1_accuracy > set2_accuracy  ~ set1_accuracy,
      set2_accuracy > set1_accuracy  ~ set2_accuracy,
      set1_accuracy == set2_accuracy ~ 999)) %>%
    mutate(winning_accuracy = ifelse(winning_accuracy == 999, NA, winning_accuracy)) %>%
    filter(!is.na(winning_accuracy)) %>%
    group_by(problem) %>%
    slice_max(order_by = winning_accuracy, n = 1) %>%
    ungroup() %>%
    mutate(winning_accuracy = case_when(
      set1_accuracy > set2_accuracy  ~ set1_accuracy,
      set2_accuracy > set1_accuracy  ~ set2_accuracy)) %>%
    mutate(winner_bench = ifelse(set1_accuracy == winning_accuracy, set1, set2)) %>%
    dplyr::select(c(problem, winner_bench)) %>%
    distinct() %>%
    group_by(problem) %>%
    mutate(winner_id = paste0("winner_", row_number())) %>%
    ungroup() %>%
    pivot_wider(id_cols = "problem", names_from = "winner_id", values_from = "winner_bench") %>%
    mutate(winner_bench = case_when(
            is.na(winner_2)                    ~ winner_1,
            !is.na(winner_2) & is.na(winner_3) ~ paste(winner_1, winner_2, sep = "/"),
            TRUE                               ~ paste(winner_1, winner_2, winner_3, winner_4, 
                                                       winner_5, winner_6, winner_7, winner_8,
                                                       winner_9, winner_10, winner_11, sep = "/"))) %>%
    dplyr::select(c(problem, winner_1, winner_bench)) # Where there is a tie we just pick one as each winner got 100% across the board
  
  # Merge
  
  winners <- comps_feats %>%
    inner_join(comps_benches, by = c("problem" = "problem"))
  
  # Filter overall results by "best of" lists and determine significance
  
  comps3 <- comps2 %>%
    left_join(winners, by = c("problem" = "problem")) %>%
    mutate(flag = case_when(
            set1 == winner_set & set2 == winner_bench ~ TRUE,
            set2 == winner_set & set1 == winner_bench ~ TRUE,
            TRUE                                      ~ FALSE)) %>% 
    filter(flag) %>%
    dplyr::select(-c(flag)) %>%
    mutate(flag = case_when(
      is.na(p_value)                                  ~ "Zero variance for one/more sets",
      p_value > .05                                   ~ "Non-Significant difference",
      p_value < .05 & set1_accuracy > set2_accuracy   ~ set1,
      p_value < .05 & set1_accuracy < set2_accuracy   ~ set2),
      flag_adj = case_when(
        is.na(p_value_adj)                                ~ "Zero variance for one/more sets",
        p_value_adj > .05                                 ~ "Non-Significant difference",
        p_value_adj < .05 & set1_accuracy > set2_accuracy ~ set1,
        p_value_adj < .05 & set1_accuracy < set2_accuracy ~ set2))

  return(comps3)
}

winners <- find_winners(outputs_data = outputs, outputs_agg_data = outputs_aggregate, benchmark_data = benchmarks)

#--------------------- Draw plots ------------------

# Define coordinates for upper triangle to shade

upper_tri <- data.frame(x = c(0, 0, 100), y = c(0, 100, 100))

# Quickly arrange into "benchmark" and "features" columns for plotting then re-join

set_data1 <- winner %>%
  mutate(set_ind = ifelse(set1 %in% append(unique(outputs$method), "All features"),
                          TRUE, FALSE)) %>%
  filter(set_ind) %>%
  dplyr::select(c(problem, set1, set1_accuracy, flag, flag_adj)) %>%
  rename(method = set1,
         set_accuracy = set1_accuracy)

set_data2 <- winner %>%
  mutate(set_ind = ifelse(set2 %in% append(unique(outputs$method), "All features"),
                          TRUE, FALSE)) %>%
  filter(set_ind) %>%
  dplyr::select(c(problem, set2, set2_accuracy, flag, flag_adj)) %>%
  rename(method = set2,
         set_accuracy = set2_accuracy)

bench_data1 <- winner %>%
  mutate(set_ind = ifelse(set1 %in% unique(benchmarks$method),
                          TRUE, FALSE)) %>%
  filter(set_ind) %>%
  dplyr::select(c(problem, set1, set1_accuracy, flag, flag_adj)) %>%
  rename(method_bench = set1,
         bench_accuracy = set1_accuracy)

bench_data2 <- winner %>%
  mutate(set_ind = ifelse(set2 %in% unique(benchmarks$method),
                          TRUE, FALSE)) %>%
  filter(set_ind) %>%
  dplyr::select(c(problem, set2, set2_accuracy, flag, flag_adj)) %>%
  rename(method_bench = set2,
         bench_accuracy = set2_accuracy)

set_final <- bind_rows(set_data1, set_data2)
bench_final <- bind_rows(bench_data1, bench_data2)

# Retrieve mean accuracy and +/- 1 SD bars for each problem to plot

set_bars <- outputs_aggregate %>%
  mutate(method = "All features") %>%
  bind_rows(outputs) %>%
  inner_join(set_final, by = c("problem" = "problem",
                               "method" = "method")) %>%
  group_by(problem, method, set_accuracy, flag, flag_adj) %>%
  summarise(mean_x = mean(accuracy, na.rm = TRUE),
            lower_x = mean(accuracy, na.rm = TRUE) - 1 * sd(accuracy, na.rm = TRUE),
            upper_x = mean(accuracy, na.rm = TRUE) + 1 * sd(accuracy, na.rm = TRUE)) %>%
  ungroup()

bench_bars <- benchmarks %>%
  inner_join(bench_final, by = c("problem" = "problem",
                                 "method" = "method_bench")) %>%
  group_by(problem, method, bench_accuracy, flag, flag_adj) %>%
  summarise(mean_y = mean(accuracy, na.rm = TRUE),
            lower_y = mean(accuracy, na.rm = TRUE) - 1 * sd(accuracy, na.rm = TRUE),
            upper_y = mean(accuracy, na.rm = TRUE) + 1 * sd(accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(method_bench = method)

winner_final <- set_bars %>%
  inner_join(bench_bars, by = c("problem" = "problem",
                                "flag" = "flag", "flag_adj" = "flag_adj"))

# Draw scatterplot

p <- winner_final %>%
  mutate(across(c(mean_x, lower_x, upper_x,
                  mean_y, lower_y, upper_y), ~ .x * 100)) %>%
  ggplot(aes(x = mean_x, y = mean_y)) +
  geom_polygon(data = upper_tri, aes(x = x, y = y), fill = "steelblue2", alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, colour = "grey50", lty = "dashed") +
  geom_errorbar(aes(ymin = lower_y, ymax = upper_y, colour = flag_adj)) +
  geom_errorbarh(aes(xmin = lower_x, xmax = upper_x, colour = flag_adj)) +
  geom_point(aes(colour = flag_adj), size = 2) +
  annotate("text", x = 80, y = 10, label = "Time-series features better") +
  annotate("text", x = 20, y = 90, label = "Leading benchmark better") +
  labs(title = "Comparison of feature sets versus benchmark algorithms across UCR/UEA repository univariate problems",
       subtitle = "Plots a subset of 50 problems where preliminary analysis showed mean and variance did not outperform chance",
       x = "Classification accuracy time-series features (%)",
       y = "Classification accuracy benchmark algorithm (%)",
       caption = "Statistical significance computed on Holm-Bonferroni corrected p-values across every pairwise combination of problems/feature sets/benchmark algorithms.",
       colour = NULL) +
  scale_x_continuous(labels = function(x)paste0(x, "%")) + 
  scale_y_continuous(labels = function(x)paste0(x, "%")) + 
  scale_colour_manual(values = RColorBrewer::brewer.pal(11, "Paired")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank())

print(p)
ggsave("output/non-z-scored/features-vs-bench.pdf", p)
