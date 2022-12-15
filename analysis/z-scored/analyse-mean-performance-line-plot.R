#-----------------------------------------
# This script sets out to plot overall
# performance for each set across problems
#-----------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 15 September 2022
#-------------------------------------------

# Load classification results

load("data/outputs_z.Rda")

outputs_z <- outputs_z %>%
  mutate(method = case_when(
    method == "tsfel" ~ "TSFEL",
    method == "kats"  ~ "Kats",
    TRUE              ~ method))

#---------------------- Calculations ----------------------

calcs <- outputs_z %>%
  group_by(problem, method) %>%
  summarise(avg = mean(balanced_accuracy, na.rm = TRUE),
            stddev = sd(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(lower = avg - 2 * stddev,
         upper = avg + 2 * stddev)

# Order for plot

orders <- outputs_z %>%
  group_by(problem) %>%
  summarise(avg = mean(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(orders = dense_rank(avg)) %>%
  dplyr::select(c(problem, orders))

calcs <- calcs %>%
  left_join(orders, by = c("problem" = "problem"))

#---------------------- Plotting ----------------------

mypal <- c("catch22" = mypal[1],
           "feasts" = mypal[2],
           "Kats" = mypal[3],
           "tsfeatures" = mypal[4],
           "TSFEL" = mypal[5],
           "tsfresh" = mypal[6])

p <- calcs %>%
  ggplot(aes(x = reorder(problem, -orders), y = avg, group = method, colour = method)) +
  geom_line() +
  labs(title = "Mean balanced accuracy across all problems by feature set",
       x = "Problem",
       y = "Mean balanced accuracy",
       colour = NULL) +
  scale_colour_manual(values = mypal) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90))

print(p)
ggsave("output/z-scored/mean-performance-line-plot.pdf", p, unit = "in", width = 15, height = 10)
