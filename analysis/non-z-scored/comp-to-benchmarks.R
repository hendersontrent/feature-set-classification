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

# Get problems where mean and variance did not outperform chance

load("ftm_non_sig.Rda")

# Grab benchmark results

benchmarks <- pull_benchmark_results() %>%
  filter(problem %in% ftm_non_sig)

# Find best per problem

benchmarks_avg <- benchmarks %>%
  group_by(problem, method) %>%
  summarise(accuracy = mean(accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(problem) %>%
  mutate(ranker = dense_rank(-accuracy)) %>%
  ungroup() %>%
  filter(ranker == 1) %>%
  dplyr::select(-c(ranker, accuracy)) %>%
  mutate(flag = TRUE) %>%
  group_by(problem) %>%
  mutate(names = ifelse(n() > 1, paste(method, collapse = "/"), method)) %>%
  ungroup()

#----------------------------------------
# Handle ties and extract accuracy values
#----------------------------------------

# No ties

no_ties <- benchmarks_avg %>%
  filter(!grepl("\\/", names))

benchmarks_no_ties <- benchmarks %>%
  inner_join(no_ties, by = c("problem" = "problem", "method" = "method")) %>%
  dplyr::select(-c(flag, names))

# Ties

ties_not_100 <- benchmarks_avg %>%
  filter(grepl("\\/", names))

benchmarks_ties <- benchmarks %>%
  inner_join(ties_not_100, by = c("problem" = "problem", "method" = "method")) %>%
  group_by(problem) %>%
  arrange(method) %>%
  slice_min(order_by = method, n = 1) %>% # Take earliest in alphabet
  ungroup() %>%
  dplyr::select(-c(method, flag)) %>%
  rename(method = names)

benchmarks <- bind_rows(benchmarks_no_ties, benchmarks_ties)
rm(benchmarks_avg, no_ties, benchmarks_no_ties, ties_not_100, benchmarks_ties)

#---------------- Compute best feature set ----------------

 # Load classification results for feature sets and compute winner

load("data/outputs.Rda")

main_models <- outputs %>%
  group_by(problem, method) %>%
  summarise(mean_accuracy = mean(accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(problem) %>%
  mutate(ranker = dense_rank(-mean_accuracy)) %>%
  ungroup() %>%
  filter(ranker == 1) %>%
  dplyr::select(-c(ranker)) %>%
  filter(problem %in% ftm_non_sig)

outputs_filt <- outputs %>%
  inner_join(main_models, by = c("problem" = "problem", "method" = "method")) %>%
  dplyr::select(c(problem, method, accuracy))

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
    dplyr::select(c(problem, set1, set2, set1_accuracy, set2_accuracy, statistic, p.value)) %>%
    filter(!is.na(set1_accuracy) & !is.na(set2_accuracy)) %>%
    mutate(p.value.adj = p.adjust(p.value, method = "holm")) %>%
    mutate(flag = case_when(
      is.na(p.value)                                  ~ "Zero variance for one/more sets",
      p.value >= .05                                  ~ "Non-significant difference",
      p.value < .05 & set1_accuracy > set2_accuracy   ~ set1,
      p.value < .05 & set1_accuracy < set2_accuracy   ~ set2))

  return(comps2)
}

winners <- find_winners(outputs_data = outputs_filt, benchmark_data = benchmarks)

#--------------------- Draw plots ------------------

# Retrieve mean accuracy and +/- 1 SD bars for each problem to plot

set_bars <- outputs_filt %>%
  group_by(problem, method) %>%
  summarise(mean_x = mean(accuracy, na.rm = TRUE),
            lower_x = mean(accuracy, na.rm = TRUE) - 1 * sd(accuracy, na.rm = TRUE),
            upper_x = mean(accuracy, na.rm = TRUE) + 1 * sd(accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(method_set = method)

bench_bars <- benchmarks %>%
  group_by(problem, method) %>%
  summarise(mean_y = mean(accuracy, na.rm = TRUE),
            lower_y = mean(accuracy, na.rm = TRUE) - 1 * sd(accuracy, na.rm = TRUE),
            upper_y = mean(accuracy, na.rm = TRUE) + 1 * sd(accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(method_bench = method)

bars <- set_bars %>%
  inner_join(bench_bars, by = c("problem" = "problem"))

winners <- winners %>%
  inner_join(bars, by = c("problem" = "problem"))

rm(set_bars, bench_bars, bars)

# Define coordinates for upper triangle to shade

upper_tri <- data.frame(x = c(0, 0, 100), y = c(0, 100, 100))

# Separate into significant and non-significant for point sizing

'%ni%' <- Negate('%in%')

winners <- winners %>%
  mutate(across(c(mean_x, lower_x, upper_x,
                  mean_y, lower_y, upper_y), ~ .x * 100))

ns <- winners %>%
  filter(flag %in% c("Non-significant difference", "Zero variance for one/more sets"))

sig <- winners %>%
  filter(flag %ni% c("Non-significant difference", "Zero variance for one/more sets"))

stopifnot(nrow(ns) + nrow(sig) == nrow(winners))

# Define colour palette

mypal2 <- c("Non-significant difference" = "grey80",
            "Zero variance for one/more sets" = "grey50",
            "HIVE-COTE" = mypal[1],
            "ST" = mypal[2],
            "Flat-COTE" = mypal[3])

# Draw scatterplot

p <- ns %>%
  ggplot(aes(x = mean_x, y = mean_y)) +
  geom_polygon(data = upper_tri, aes(x = x, y = y), fill = "steelblue2", alpha = 0.1) +
  geom_abline(intercept = 0, slope = 1, colour = "grey50", lty = "dashed") +
  geom_errorbar(aes(ymin = lower_y, ymax = upper_y, colour = flag)) +
  geom_errorbarh(aes(xmin = lower_x, xmax = upper_x, colour = flag)) +
  geom_point(aes(colour = flag), size = 2) +
  geom_linerange(data = sig, aes(ymin = lower_y, ymax = upper_y, colour = flag)) +
  geom_linerange(data = sig, aes(xmin = lower_x, xmax = upper_x, colour = flag)) +
  geom_point(data = sig, aes(colour = flag), size = 3) +
  geom_text_repel(data = sig, aes(label = problem), legend = FALSE, segment.linetype = "dashed", box.padding = 1.25, max.overlaps = Inf) +
  annotate("text", x = 80, y = 10, label = "Best time-series feature set better", size = 4, fontface = 2) +
  annotate("text", x = 20, y = 90, label = "Best benchmark algorithm better", size = 4, fontface = 2) +
  labs(x = "Classification accuracy time-series features (%)",
       y = "Classification accuracy benchmark algorithm (%)",
       colour = NULL) +
  scale_x_continuous(labels = function(x)paste0(x, "%")) + 
  scale_y_continuous(labels = function(x)paste0(x, "%")) + 
  scale_colour_manual(values = mypal2) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 11))

print(p)
ggsave("output/non-z-scored/features-vs-bench.pdf", p, units = "in", height = 11, width = 11)
