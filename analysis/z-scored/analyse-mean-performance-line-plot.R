#-----------------------------------------
# This script sets out to plot overall
# performance for each set across problems
#-----------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 15 September 2022
#-------------------------------------------

# Load classification results

load("data/outputs_z.Rda")

#---------------------- Calculations ----------------------

calcs <- outputs_z %>%
  group_by(problem, method) %>%
  summarise(avg = mean(accuracy, na.rm = TRUE),
            stddev = sd(accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(lower = avg - 2 * stddev,
         upper = avg + 2 * stddev)

# Order for plot

orders <- outputs_z %>%
  group_by(problem) %>%
  summarise(avg = mean(accuracy, na.rm = TRUE)) %>%
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
  mutate(avg = avg * 100) %>%
  ggplot(aes(x = reorder(problem, -orders), y = avg, group = method, colour = method)) +
  geom_line() +
  geom_point(size = 0.7) +
  labs(x = "Problem",
       y = "Mean classification accuracy (%)",
       colour = NULL) +
  scale_y_continuous(labels = function(x)paste0(x, "%")) + 
  scale_colour_manual(values = mypal) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11))

print(p)
ggsave("output/z-scored/mean-performance-line-plot.pdf", p, unit = "in", width = 15, height = 10)
