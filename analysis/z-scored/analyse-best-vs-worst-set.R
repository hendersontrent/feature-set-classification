#------------------------------------------
# This script sets out to produce analysis
# of best versus worst feature set by 
# problem
#
# NOTE: This script requires setup.R and
# analysis/compute-features_z-score.R and
# analysis/fit-classifiers_z-score.R to have 
# been run first
#------------------------------------------

#--------------------------------------
# Author: Trent Henderson, 21 July 2022
#--------------------------------------

# Load classification results

load("data/outputs_z.Rda")

# Find best feature set by problem

best <- outputs_z %>%
  mutate(accuracy = accuracy * 100) %>%
  group_by(problem, method) %>%
  summarise(best_accuracy_mean = mean(accuracy, na.rm = TRUE),
            best_accuracy_sd = sd(accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(problem) %>%
  mutate(ranker = dense_rank(-best_accuracy_mean)) %>%
  ungroup() %>%
  filter(ranker == 1) %>%
  dplyr::select(-c(ranker)) %>%
  mutate(flag = ifelse(problem == "Plane" & method == "TSFEL", TRUE, FALSE)) %>% # tsfeatures and TSFEL had the same values, remove duplicate
  filter(!flag) %>%
  dplyr::select(-c(flag)) %>%
  rename(best_method = method)

# Find worst feature set by problem

worst <- outputs_z %>%
  mutate(accuracy = accuracy * 100) %>%
  group_by(problem, method) %>%
  summarise(accuracy_mean = mean(accuracy, na.rm = TRUE),
            accuracy_sd = sd(accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(problem) %>%
  mutate(ranker = dense_rank(-accuracy_mean)) %>%
  mutate(the_max = max(ranker)) %>%
  filter(ranker == the_max) %>% # As there is a tie
  mutate(the_min = min(accuracy_sd)) %>%
  filter(accuracy_sd == the_min) %>% # As there are ties
  ungroup() %>%
  dplyr::select(-c(ranker, the_max, the_min)) %>%
  rename(worst_method = method,
         worst_accuracy_mean = accuracy_mean,
         worst_accuracy_sd = accuracy_sd)

both <- best %>%
  inner_join(worst, by = c("problem" = "problem")) %>%
  mutate(lower_x = worst_accuracy_mean - 1 * worst_accuracy_sd,
         upper_x = worst_accuracy_mean + 1 * worst_accuracy_sd,
         lower_y = best_accuracy_mean - 1 * best_accuracy_sd,
         upper_y = best_accuracy_mean + 1 * best_accuracy_sd)

#---------------------- Calculate p-values -----------------------

p_values <- unique(both$problem) %>%
  purrr::map_df(~ calculate_p_values(data = outputs_z, summary_data = both, theproblem = .x, all_features = FALSE, problem_data = problem_summaries))

both <- both %>%
  inner_join(p_values, by = c("problem" = "problem")) %>%
  mutate(p.value.adj = p.adjust(p.value, method = "holm")) %>%
  mutate(significant = ifelse(p.value < 0.05, "Significant difference", "Non-significant difference"),
         top_performer = ifelse(significant == "Significant difference", best_method, "Non-significant difference")) %>%
  mutate(significant = ifelse(best_accuracy_sd == 0 | worst_accuracy_sd == 0, "Zero variance for one/more sets", significant),
         top_performer = ifelse(best_accuracy_sd == 0 | worst_accuracy_sd == 0, "Zero variance for one/more sets", top_performer))

rm(outputs_z, best, worst)

#---------------------- Draw summary graphic ---------------------

# Create palette for whoever is top performer

mypal2 <- c("Non-significant difference" = "grey80",
            "Zero variance for one/more sets" = "grey50",
            "catch22" = mypal[1],
            "feasts" = mypal[2],
            "Kats" = mypal[3],
            "tsfeatures" = mypal[4],
            "TSFEL" = mypal[5],
            "tsfresh" = mypal[6])

# Define coordinates for upper triangle to shade

upper_tri <- data.frame(x = c(0, 0, 100), y = c(0, 100, 100))

# Separate into significant and non-significant for point sizing

ns <- both %>%
  filter(top_performer %in% c("Non-significant difference", "Zero variance for one/more sets"))

sig <- both %>%
  filter(top_performer %in% c("catch22", "feasts", "Kats", "tsfeatures", "TSFEL", "tsfresh"))

stopifnot(nrow(ns) + nrow(sig) == nrow(both))

# Draw scatterplot

p <- ns %>%
  ggplot(aes(x = worst_accuracy_mean, y = best_accuracy_mean)) +
  geom_polygon(data = upper_tri, aes(x = x, y = y), fill = "steelblue2", alpha = 0.1) +
  geom_abline(intercept = 0, slope = 1, colour = "grey50", lty = "dashed") +
  geom_errorbar(aes(ymin = lower_y, ymax = upper_y, colour = top_performer)) +
  geom_errorbarh(aes(xmin = lower_x, xmax = upper_x, colour = top_performer)) +
  geom_point(aes(colour = top_performer), size = 2) +
  geom_linerange(data = sig, aes(ymin = lower_y, ymax = upper_y, colour = top_performer), size = 0.7) +
  geom_linerange(data = sig, aes(xmin = lower_x, xmax = upper_x, colour = top_performer), size = 0.7) +
  geom_point(data = sig, aes(colour = top_performer), size = 3) +
  geom_text_repel(data = sig, aes(label = problem), legend = FALSE, segment.linetype = "dashed") +
  annotate("text", x = 75, y = 10, label = "Worst feature set", size = 4, fontface = 2) +
  annotate("text", x = 25, y = 90, label = "Best feature set", size = 4, fontface = 2) +
  labs(x = "Classification accuracy worst set (%)",
       y = "Classification accuracy best set (%)",
       colour = NULL) +
  scale_x_continuous(labels = function(x)paste0(x, "%")) + 
  scale_y_continuous(labels = function(x)paste0(x, "%")) + 
  scale_colour_manual(values = mypal2) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11))

print(p)
ggsave("output/z-scored/best_versus_worst_set.pdf", p, units = "in", height = 20, width = 20)
