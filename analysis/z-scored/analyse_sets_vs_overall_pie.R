#-------------------------------------------
# This script sets out to produce a creative
# graphic to visualise individual sets
# against all features
#
# NOTE: This script requires setup.R and
# analysis/compute-features_z-score.R and
# analysis/fit-classifiers_z-score.R 
# to have been run first
#-------------------------------------------

#--------------------------------------
# Author: Trent Henderson, 29 July 2022
#--------------------------------------

# Load classification results

load("data/outputs_z.Rda")
load("data/outputs_z_bp.Rda")
load("data/outputs_aggregate_z.Rda")
outputs_z <- bind_rows(outputs_z, outputs_z_bp)

# Calculate winners

main_models <- outputs_z %>%
  mutate(accuracy = accuracy * 100) %>%
  group_by(problem, method) %>%
  summarise(accuracy_mean = mean(accuracy, na.rm = TRUE),
            accuracy_sd = sd(accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(method_set = method) %>%
  group_by(problem) %>%
  mutate(ranker = dense_rank(-accuracy_mean)) %>%
  ungroup() %>%
  filter(ranker == 1) %>%
  dplyr::select(-c(ranker)) %>%
  mutate(flag = ifelse(problem == "Plane" & method_set == "TSFEL", TRUE, FALSE)) %>% # tsfeatures and TSFEL had the same values, remove duplicate
  filter(!flag) %>%
  dplyr::select(-c(flag)) %>%
  rename(accuracy = accuracy_mean)

main_models_aggregate <- outputs_aggregate_z %>%
  mutate(accuracy = accuracy * 100,
         method = "All features") %>%
  group_by(problem, method) %>%
  summarise(accuracy_all = mean(accuracy, na.rm = TRUE),
            accuracy_sd_all = sd(accuracy, na.rm = TRUE)) %>%
  ungroup()

# Join the datasets and compute bars for plot + the top overall performer

all_mains <- main_models %>%
  left_join(main_models_aggregate, by = c("problem" = "problem")) %>%
  mutate(lower_x = accuracy - 1 * accuracy_sd,
         upper_x = accuracy + 1 * accuracy_sd,
         lower_y = accuracy_all - 1 * accuracy_sd_all,
         upper_y = accuracy_all + 1 * accuracy_sd_all) %>%
  mutate(top_performer = ifelse(accuracy > accuracy_all, method_set, method)) %>%
  drop_na()

#---------------------- Calculate p-values -----------------------

# Set up single dataframe

reduced <- outputs_z %>%
  dplyr::select(c(problem, method, accuracy))

reduced2 <- outputs_aggregate_z %>%
  mutate(method = "All features") %>%
  dplyr::select(c(problem, method, accuracy))

reduced <- bind_rows(reduced, reduced2)

# Iterate over every pairwise comparison to all features for each problem

combns <- crossing(unique(outputs_z$method), c("All features")) %>%
  rename(set1 = 1, set2 = 2)

problem <- rep(unique(reduced$problem), times = 1, each = 8)
combns <- combns[rep(1:nrow(combns), length(unique(reduced$problem)),), ]
combns <- cbind(combns, problem)

comps <- 1:nrow(combns) %>%
  purrr::map_df(~ calculate_p_values2(data = reduced, combn_data = combns, rownum = .x, problem_data = problem_summaries))

# Add in accuracy values to get direction

single_accs <- outputs_z %>%
  mutate(accuracy = accuracy * 100,
         accuracy = accuracy * 100) %>%
  group_by(problem, method) %>%
  summarise(accuracy_mean = mean(accuracy, na.rm = TRUE)) %>%
  ungroup()

all_accs <- outputs_aggregate_z %>%
  mutate(accuracy = accuracy * 100,
         accuracy = accuracy * 100) %>%
  group_by(problem) %>%
  summarise(accuracy_all = mean(accuracy, na.rm = TRUE)) %>%
  ungroup()

comps <- comps %>%
  group_by(problem) %>%
  mutate(p.value.adj = p.adjust(p.value, method = "holm")) %>%
  ungroup() %>%
  inner_join(single_accs, by = c("problem" = "problem", "method" = "method")) %>%
  inner_join(all_accs, by = c("problem" = "problem")) %>%
  mutate(flag = case_when(
         is.na(p.value)                                                 ~ "Zero variance for one/more sets",
         p.value >= .05                                                  ~ "Non-significant difference",
         p.value < .05 & accuracy_mean > accuracy_all ~ method,
         TRUE                                                           ~ "All features"))

#---------------------- Set up final dataframe -------------------

#-------------------------------
# Make wide dataframe of results 
# to feed into pie graphs
#-------------------------------

# Individual set wins

wide <- comps %>%
  mutate(accuracy_mean = ifelse(flag %in% c("All features", "Zero variance for one/more sets", "Non-significant difference"),
                                         0, accuracy_mean)) %>%
  dplyr::select(c(problem, method, accuracy_mean)) %>%
  mutate(accuracy_mean = accuracy_mean / 100) %>%
  pivot_wider(id_cols = "problem", names_from = "method", values_from = accuracy_mean)

# All other cases

main_models_filt <- main_models %>%
  dplyr::select(c(problem, method_set))

wide2 <- comps %>%
  filter(flag %in% c("All features", "Zero variance for one/more sets", "Non-significant difference")) %>%
  mutate(`All features` = ifelse(flag == "All features", 1, 0),
         `Zero variance for one/more sets` = ifelse(flag == "Zero variance for one/more sets", 1, 0),
         `Non-significant difference` = ifelse(flag == "Non-significant difference", 1, 0)) %>%
  dplyr::select(c(problem, method, `All features`, `Zero variance for one/more sets`, `Non-significant difference`)) %>%
  inner_join(main_models_filt, by = c("problem" = "problem", "method" = "method_set")) %>%
  dplyr::select(c(problem, `All features`, `Zero variance for one/more sets`, `Non-significant difference`))

# Join

wide <- wide %>%
  left_join(wide2, by = c("problem" = "problem"))

# Add indicator for beating all features to core dataframe

all_mains2 <- all_mains %>%
  inner_join(wide, by = c("problem" = "problem")) %>%
  mutate(top_performer = case_when(
          `Non-significant difference` == 1      ~ "Non-significant difference",
          `Zero variance for one/more sets` == 1 ~ "Zero variance for one/more sets",
          TRUE                                   ~ top_performer))

all_mains2[is.na(all_mains2)] <- 0 # Gets pie graph to work

#---------------------- Separate dataframes for plot control -------------------

# Find number of unique values to enable filtering

separates <- all_mains2 %>%
  rowwise %>%
  mutate(uniques = n_distinct(c_across(catch22:tsfresh))) %>%
  ungroup() %>%
  mutate(my_label = ifelse(top_performer %in% c("Non-significant difference", "Zero variance for one/more sets"), NA, problem)) %>%
  mutate()

# Rows where we just want a geom_point

point_df <- separates %>%
  filter(top_performer %in% c("Non-significant difference", "Zero variance for one/more sets"))

# Rows where we want a geom_scatterpie

pie_df <- separates %>%
  filter(top_performer %ni% c("Non-significant difference", "Zero variance for one/more sets"))

stopifnot(nrow(point_df) + nrow(pie_df) == nrow(separates)) # Small unit test

#---------------------- Produce graphic --------------------------

# Create palette for whoever is top performer

mypal2 <- c("All features" = "black",
            "Non-significant difference" = "grey80",
            "Zero variance for one/more sets" = "grey50",
            "catch22" = mypal[1],
            "feasts" = mypal[2],
            "Kats" = mypal[3],
            "tsfeatures" = mypal[4],
            "TSFEL" = mypal[5],
            "tsfresh" = mypal[6],
            "fft" = mypal[7],
            "quantiles" = mypal[8])

# Define coordinates for upper triangle to shade

upper_tri <- data.frame(x = c(0, 0, 100), y = c(0, 100, 100))

# Draw scatterplot

p <- point_df %>%
  ggplot(aes(x = accuracy, y = accuracy_all)) +
  geom_polygon(data = upper_tri, aes(x = x, y = y), fill = "steelblue2", alpha = 0.1) +
  geom_abline(intercept = 0, slope = 1, colour = "grey50", lty = "dashed") +
  geom_errorbar(aes(ymin = lower_y, ymax = upper_y, colour = top_performer), size = 0.7) +
  geom_errorbarh(aes(xmin = lower_x, xmax = upper_x, colour = top_performer), size = 0.7) +
  geom_point(aes(colour = top_performer), size = 2) +
  geom_linerange(data = pie_df, aes(ymin = lower_y, ymax = upper_y, colour = top_performer), size = 0.7) +
  geom_linerange(data = pie_df, aes(xmin = lower_x, xmax = upper_x, colour = top_performer), size = 0.7) +
  geom_scatterpie(aes(x = accuracy, y = accuracy_all), data = pie_df, pie_scale = 2,
                  cols = colnames(all_mains2)[13:length(colnames(all_mains2))], alpha = 0.8) +
  geom_text_repel(aes(label = my_label), legend = FALSE, segment.linetype = "dashed", box.padding = 1.25) +
  geom_text_repel(data = pie_df, aes(x = accuracy, y = accuracy_all, label = my_label), legend = FALSE, 
                  segment.linetype = "dashed", box.padding = 1.25) +
  annotate("text", x = 75, y = 10, label = "Best single feature set better", size = 4, fontface = 2) +
  annotate("text", x = 25, y = 90, label = "All features better", size = 4, fontface = 2) +
  labs(x = "Classification accuracy of the best individual set (%)",
       y = "Classification accuracy of all features (%)",
       fill = NULL,
       group = NULL) +
  scale_x_continuous(labels = function(x)paste0(x, "%")) + 
  scale_y_continuous(labels = function(x)paste0(x, "%")) + 
  scale_fill_manual(values = mypal2, drop = FALSE) +
  scale_colour_manual(values = mypal2, guide = "none") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11))

print(p)
ggsave("output/z-scored/all_versus_sets_pie.pdf", p, units = "in", height = 10, width = 10)
