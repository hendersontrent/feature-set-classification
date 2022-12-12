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

# Grab benchmark results and find best per problem

benchmarks <- pull_benchmark_results() %>%
  dplyr::select(c(problem, method, accuracy))

benchmarks_avg <- benchmarks%>%
  group_by(problem, method) %>%
  summarise(accuracy = mean(accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(problem) %>%
  mutate(ranker = dense_rank(-accuracy)) %>%
  ungroup() %>%
  filter(ranker == 1) %>%
  dplyr::select(-c(ranker, accuracy)) %>%
  mutate(flag = TRUE)

benchmarks <- benchmarks %>%
  inner_join(benchmarks_avg, by = c("problem" = "problem", "method" = "method")) %>%
  dplyr::select(-c(flag))

#---------------- Compute best feature set ----------------

 # Load classification results for feature sets and compute winner

load("data/outputs.Rda")
load("data/outputs_aggregate.Rda")

outputs <- outputs %>%
  mutate(method = case_when(
    method == "tsfel" ~ "TSFEL",
    method == "kats"  ~ "Kats",
    TRUE              ~ method))

main_models <- outputs %>%
  group_by(problem, method) %>%
  summarise(accuracy = mean(accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(problem) %>%
  mutate(ranker = dense_rank(-accuracy)) %>%
  ungroup() %>%
  filter(ranker == 1) %>%
  dplyr::select(-c(ranker))

main_models_aggregate <- outputs_aggregate %>%
  mutate(method = "All features") %>%
  group_by(problem, method) %>%
  summarise(accuracy = mean(accuracy, na.rm = TRUE)) %>%
  ungroup()

main_models <- bind_rows(main_models, main_models_aggregate) %>%
  group_by(problem) %>%
  slice_max(order_by = accuracy, n = 1) %>%
  ungroup() %>%
  dplyr::select(c(problem, method)) %>%
  mutate(keeper = TRUE)

outputs_filt <- outputs %>%
  dplyr::select(c(problem, method, accuracy))

outputs_filt_aggregate <- outputs_aggregate %>%
  mutate(method = "All features") %>%
  dplyr::select(c(problem, method, accuracy))

outputs_filt <- bind_rows(outputs_filt, outputs_filt_aggregate) %>%
  inner_join(main_models, by = c("problem" = "problem", "method" = "method")) %>%
  dplyr::select(-c(keeper))

rm(outputs, outputs_aggregate, main_models, main_models_aggregate, outputs_filt_aggregate)

# Filter benchmarks to just problems in calculated sets

benchmarks <- benchmarks %>%
  filter(problem %in% unique(outputs_filt$problem))

# Compare best feature set and benchmark

#' Find best set for each problem
#' @param outputs_data \code{data.frame} of best feature set results
#' @param benchmark_data \code{data.frame} of benchmark algorithm results
#' @return object of class \code{data.frame}
#' @author Trent Henderson
#' 

find_winners <- function(outputs_data, benchmark_data){
  
  '%ni' <- Negate('%in%')
  
  #---------------------- Calculate p-values -----------------------
  
  all_data <- bind_rows(outputs_data, benchmark_data)
  
  # Get pairwise combinations to iterate over
  
  combns <- crossing(unique(all_data$method), unique(all_data$method), unique(all_data$problem), .name_repair = "unique") %>%
    rename(set1 = 1, 
           set2 = 2,
           problem = 3)
  
  combns <- combns[!duplicated(data.frame(t(apply(combns, 1, sort)))), ] # Remove duplicates since we get both set's values in the function
  combns <- combns[combns$set1 != combns$set2, ]
  
  combns <- combns %>%
    mutate(flag = ifelse(set1 %ni% unique(outputs_data$method) & set2 %ni% unique(outputs_data$method), FALSE, TRUE)) %>%
    filter(flag) %>%
    dplyr::select(-c(flag))
  
  correct_pairs <- outputs_data %>%
    dplyr::select(-c(accuracy)) %>%
    mutate(flag = TRUE) %>%
    distinct()
  
  correct_pairs2 <- outputs_data %>%
    dplyr::select(-c(accuracy)) %>%
    mutate(flag2 = TRUE) %>%
    distinct()
  
  combns <- combns %>%
    left_join(correct_pairs, by = c("problem" = "problem", "set1" = "method")) %>%
    left_join(correct_pairs2, by = c("problem" = "problem", "set2" = "method")) %>%
    filter(flag | flag2) %>%
    mutate(flag3 = ifelse((set1 %in% unique(outputs_data$method) & set2 %ni% unique(outputs_data$method)) |
                            (set2 %in% unique(outputs_data$method) & set1 %ni% unique(outputs_data$method)),
                          TRUE, FALSE)) %>%
    filter(flag3) %>%
    dplyr::select(-c(flag, flag2, flag3))
  
  # Iterate over every pairwise comparison to all features for each problem
  
  comps <- 1:nrow(combns) %>%
    purrr::map_df(~ calculate_p_values_acc(data = all_data, combn_data = combns, rownum = .x, problem_data = problem_summaries))
  
  # Add in accuracy values to get direction
  
  feature_accs <- outputs_data %>%
    group_by(problem, method) %>%
    summarise(accuracy_set = mean(accuracy, na.rm = TRUE)) %>%
    ungroup()
  
  bench_accs <- benchmark_data %>%
    group_by(problem, method) %>%
    summarise(accuracy_bench = mean(accuracy, na.rm = TRUE)) %>%
    ungroup()
  
  sets <- unique(outputs_data$method)
  benches <- unique(benchmark_data$method)
  
  comps2 <- comps %>%
    left_join(feature_accs, by = c("problem" = "problem", "set1" = "method")) %>%
    left_join(bench_accs, by = c("problem" = "problem", "set1" = "method")) %>%
    left_join(feature_accs, by = c("problem" = "problem", "set2" = "method")) %>%
    left_join(bench_accs, by = c("problem" = "problem", "set2" = "method")) %>%
    mutate(set1_accuracy = case_when(
            set1 %in% sets         ~ accuracy_set.x,
            set1 %in% benches      ~ accuracy_bench.x)) %>%
    mutate(set2_accuracy = case_when(
      set2 %in% sets         ~ accuracy_set.y,
      set2 %in% benches      ~ accuracy_bench.y)) %>%
    dplyr::select(c(problem, set1, set2, set1_accuracy, set2_accuracy, statistic, p.value))

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
      is.na(p.value.adj)                                  ~ "Zero variance for one/more sets",
      p.value.adj > .05                                   ~ "Non-Significant difference",
      p.value.adj < .05 & set1_accuracy > set2_accuracy   ~ set1,
      p.value.adj < .05 & set1_accuracy < set2_accuracy   ~ set2))

  return(comps3)
}

winners <- find_winners(outputs_data = outputs_filt, benchmark_data = benchmarks)

#--------------------- Draw plots ------------------

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

# Define coordinates for upper triangle to shade

upper_tri <- data.frame(x = c(0, 0, 100), y = c(0, 100, 100))

# Define colour palette

mypal <- c("Non-Significant difference" = "grey50",
           "Zero variance for one/more sets" = "grey75",
           "cBOSS" = "#E41A1C",
           "HIVE-COTEv1_0" = "#377EB8",
           "InceptionTime" = "#4DAF4A",
           "ResNet" = "#984EA3",
           "ROCKET" = "#FF7F00",
           "S-BOSS" = "#FFFF33",
           "STC" = "#A65628",
           "TS-CHIEF" = "#F781BF",
           "WEASEL" = "#66C2A5")

# Draw scatterplot

p <- winner_final %>%
  mutate(across(c(mean_x, lower_x, upper_x,
                  mean_y, lower_y, upper_y), ~ .x * 100)) %>%
  mutate(flag_adj = factor(flag_adj, levels = c("Non-Significant difference",
                                                "Zero variance for one/more sets",
                                                "cBOSS", "HIVE-COTEv1_0",
                                                "InceptionTime", "ResNet",
                                                "ROCKET", "S-BOSS",
                                                "STC", "TS-CHIEF", "WEASEL"), ordered = TRUE)) %>%
  ggplot(aes(x = mean_x, y = mean_y)) +
  geom_polygon(data = upper_tri, aes(x = x, y = y), fill = "steelblue2", alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, colour = "grey50", lty = "dashed") +
  geom_errorbar(aes(ymin = lower_y, ymax = upper_y, colour = flag_adj)) +
  geom_errorbarh(aes(xmin = lower_x, xmax = upper_x, colour = flag_adj)) +
  geom_point(aes(colour = flag_adj), size = 2) +
  annotate("text", x = 80, y = 10, label = "Time-series features better") +
  annotate("text", x = 20, y = 90, label = "Leading benchmark better") +
  labs(title = "Comparison of feature sets vs benchmark algorithms across UCR/UEA univariate problems",
       subtitle = "Plots a subset of 50 problems where mean and variance did not outperform chance",
       x = "Classification accuracy time-series features (%)",
       y = "Classification accuracy benchmark algorithm (%)",
       caption = "Statistical significance computed on Holm-Bonferroni corrected p-values across every pairwise combination of\nproblems/feature sets/benchmark algorithms.",
       colour = NULL) +
  scale_x_continuous(labels = function(x)paste0(x, "%")) + 
  scale_y_continuous(labels = function(x)paste0(x, "%")) + 
  scale_colour_manual(values = mypal) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank())

print(p)
ggsave("output/non-z-scored/features-vs-bench.pdf", p)
