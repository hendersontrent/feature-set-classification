#------------------------------------------
# This script sets out to produce a creative
# graphic to visualise individual sets
# against all features
#
# NOTE: This script requires setup.R and
# analysis/compute-features.R and
# analysis/fit-classifiers.R to have been 
# run first
#-----------------------------------------

#--------------------------------------
# Author: Trent Henderson, 29 July 2022
#--------------------------------------

# Load classification results

load("data/outputs_z.Rda")
load("data/outputs_aggregate_z.Rda")

outputs_z <- outputs_z %>%
  mutate(method = case_when(
    method == "tsfel" ~ "TSFEL",
    method == "kats"  ~ "Kats",
    TRUE              ~ method))

# Calculate winners

main_models <- outputs_z %>%
  mutate(accuracy = accuracy * 100,
         balanced_accuracy = balanced_accuracy * 100) %>%
  group_by(problem, method) %>%
  summarise(balanced_accuracy_mean = mean(balanced_accuracy, na.rm = TRUE),
            balanced_accuracy_sd = sd(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(method_set = method) %>%
  group_by(problem) %>%
  mutate(ranker = dense_rank(-balanced_accuracy_mean)) %>%
  ungroup() %>%
  filter(ranker == 1) %>%
  dplyr::select(-c(ranker)) %>%
  rename(balanced_accuracy = balanced_accuracy_mean)

main_models_aggregate <- outputs_aggregate_z %>%
  mutate(accuracy = accuracy * 100,
         balanced_accuracy = balanced_accuracy * 100,
         method = "All features") %>%
  group_by(problem, method) %>%
  summarise(balanced_accuracy_all = mean(balanced_accuracy, na.rm = TRUE),
            balanced_accuracy_sd_all = sd(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup()

# Join the datasets and compute bars for plot + the top overall performer

all_mains <- main_models %>%
  left_join(main_models_aggregate, by = c("problem" = "problem")) %>%
  mutate(lower_x = balanced_accuracy - 1 * balanced_accuracy_sd,
         upper_x = balanced_accuracy + 1 * balanced_accuracy_sd,
         lower_y = balanced_accuracy_all - 1 * balanced_accuracy_sd_all,
         upper_y = balanced_accuracy_all + 1 * balanced_accuracy_sd_all) %>%
  mutate(top_performer = ifelse(balanced_accuracy > balanced_accuracy_all, method_set, method)) %>%
  drop_na()

#---------------------- Calculate p-values -----------------------

# Set up single dataframe

reduced <- outputs_z %>%
  dplyr::select(c(problem, method, balanced_accuracy))

reduced2 <- outputs_aggregate_z %>%
  mutate(method = "All features") %>%
  dplyr::select(c(problem, method, balanced_accuracy))

reduced <- bind_rows(reduced, reduced2)

# Iterate over every pairwise comparison to all features for each problem

combns <- crossing(unique(outputs_z$method), c("All features")) %>%
  rename(set1 = 1, set2 = 2)

problem <- rep(unique(reduced$problem), times = 1, each = 6)
combns <- combns[rep(1:nrow(combns), length(unique(reduced$problem)),), ]
combns <- cbind(combns, problem)

comps <- 1:nrow(combns) %>%
  purrr::map_df(~ calculate_p_values2(data = reduced, combn_data = combns, rownum = .x))

# Add in accuracy values to get direction

single_accs <- outputs_z %>%
  mutate(accuracy = accuracy * 100,
         balanced_accuracy = balanced_accuracy * 100) %>%
  group_by(problem, method) %>%
  summarise(balanced_accuracy_mean = mean(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup()

all_accs <- outputs_aggregate_z %>%
  mutate(accuracy = accuracy * 100,
         balanced_accuracy = balanced_accuracy * 100) %>%
  group_by(problem) %>%
  summarise(balanced_accuracy_all = mean(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup()

comps <- comps %>%
  inner_join(single_accs, by = c("problem" = "problem", "method" = "method")) %>%
  inner_join(all_accs, by = c("problem" = "problem")) %>%
  mutate(flag = case_when(
          is.na(p_value)                                                 ~ "Zero variance for one/more sets",
          p_value > .05                                                  ~ "Non-Significant difference",
          p_value < .05 & balanced_accuracy_mean > balanced_accuracy_all ~ method,
          TRUE                                                           ~ "All features"))

#---------------------- Set up final dataframe -------------------

#-------------------------------
# Make wide dataframe of results 
# to feed into pie graphs
#-------------------------------

# Individual set wins

wide <- comps %>%
  mutate(balanced_accuracy_mean = ifelse(flag %in% c("All features", "Zero variance for one/more sets", "Non-Significant difference"),
                                         0, balanced_accuracy_mean)) %>%
  dplyr::select(c(problem, method, balanced_accuracy_mean)) %>%
  mutate(balanced_accuracy_mean = balanced_accuracy_mean / 100) %>%
  pivot_wider(id_cols = "problem", names_from = "method", values_from = balanced_accuracy_mean)

# All other cases

main_models_filt <- main_models %>%
  dplyr::select(c(problem, method_set))

wide2 <- comps %>%
  filter(flag %in% c("All features", "Zero variance for one/more sets", "Non-Significant difference")) %>%
  mutate(`All features` = ifelse(flag == "All features", 1, 0),
         `Zero variance for one/more sets` = ifelse(flag == "Zero variance for one/more sets", 1, 0),
         `Non-significant difference` = ifelse(flag == "Non-Significant difference", 1, 0)) %>%
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

#---------------------- Produce graphic --------------------------

# Create palette for whoever is top performer

mypal <- c("All features" = "black",
           "Non-Significant difference" = "grey50",
           "Zero variance for one/more sets" = "grey75",
           "catch22" = "#1B9E77",
           "feasts" = "#D95F02",
           "Kats" = "#7570B3",
           "tsfeatures" = "#E7298A",
           "TSFEL" = "#66A61E",
           "tsfresh" = "#E6AB02")

# Define coordinates for upper triangle to shade

upper_tri <- data.frame(x = c(0, 0, 100), y = c(0, 100, 100))

# Draw scatterplot

p <- all_mains2 %>%
  ggplot(aes(x = balanced_accuracy, y = balanced_accuracy_all)) +
  geom_polygon(data = upper_tri, aes(x = x, y = y), fill = "steelblue2", alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, colour = "grey50", lty = "dashed") +
  geom_errorbar(aes(ymin = lower_y, ymax = upper_y, colour = top_performer), size = 0.7) +
  geom_errorbarh(aes(xmin = lower_x, xmax = upper_x, colour = top_performer), size = 0.7) +
  geom_scatterpie(aes(x = balanced_accuracy, y = balanced_accuracy_all), data = all_mains2,  
                   cols = colnames(all_mains2)[13:length(colnames(all_mains2))], alpha = 0.8, pie_scale = 1.25) +
  annotate("text", x = 75, y = 10, label = "Best single feature set better") +
  annotate("text", x = 25, y = 90, label = "All features better") +
  labs(title = "Comparison of top feature sets across UCR/UEA repository univariate problems",
       subtitle = "Error bars are +/- 1 SD obtained over 30 resamples for 'All features' and top individual set.\nColour indicates p < .05 difference in accuracy for individual feature set(s) over all features.\nPie proportions broadly correspond to rank, with the top performer occupying the largest space.",
       x = "Balanced classification accuracy of the best individual set (%)",
       y = "Balanced classification accuracy of all features (%)",
       fill = "Feature set",
       group = NULL) +
  scale_x_continuous(labels = function(x)paste0(x, "%")) + 
  scale_y_continuous(labels = function(x)paste0(x, "%")) + 
  scale_fill_manual(values = mypal) +
  scale_colour_manual(values = mypal, guide = "none") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

print(p)
ggsave("output/z-scored/all_versus_sets_pie.pdf", p)
