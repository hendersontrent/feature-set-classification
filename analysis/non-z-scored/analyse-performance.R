#------------------------------------------
# This script sets out to produce analysis
# of classification performance by set
# and problem
#
# NOTE: This script requires setup.R and
# analysis/compute-features.R and
# analysis/fit-classifiers.R to have been 
# run first
#-----------------------------------------

#------------------------------------
# Author: Trent Henderson, 6 May 2022
#------------------------------------

# Load classification results

load("data/outputs.Rda")

#------------------ Analysis I: Distribution of accuracies -----------------

# GOAL: Understand distribution of performance across all problems

#--------
# Boxplot
#--------

p <- outputs %>%
  ggplot(aes(x = method, y = accuracy, colour = method)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.6) +
  labs(title = "Classification accuracy by feature set across UCR/UEA repository univariate problems",
       x = "Feature set",
       y = "Classification accuracy (%)") +
  scale_y_continuous(labels = function(x)paste0(x, "%")) + 
  scale_colour_brewer(palette = "Dark2") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")

print(p)
ggsave("output/non-z-scored/boxplot.pdf", p)

#------------
# Violin plot
#------------

p1 <- outputs %>%
  ggplot(aes(x = method, y = accuracy, colour = method)) +
  geom_violin() +
  geom_jitter(alpha = 0.6) +
  labs(title = "Classification accuracy by feature set across UCR/UEA repository univariate problems",
       x = "Feature set",
       y = "Classification accuracy (%)") +
  scale_y_continuous(labels = function(x)paste0(x, "%")) + 
  scale_colour_brewer(palette = "Dark2") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")

print(p1)
ggsave("output/non-z-scored/violin.pdf", p1)

#----------
# Histogram
#----------

p2 <- outputs %>%
  ggplot(aes(x = accuracy, fill = method)) +
  geom_histogram(aes(y = ..density..), binwidth = 5) +
  labs(title = "Classification accuracy by feature set across UCR/UEA repository univariate problems",
       x = "Classification accuracy (%)",
       y = "Density") +
  scale_x_continuous(labels = function(x)paste0(x, "%")) + 
  scale_colour_brewer(palette = "Dark2") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none") +
  facet_wrap(~method)

print(p2)
ggsave("output/non-z-scored/histogram.pdf", p2)

#------------------ Analysis II: Accuracy by problem complexity ------------

# GOAL: Understand performance relative to the "difficulty" of the problem

#-----------------------------------
# Get class numbers for each problem
#-----------------------------------

# Load time series data

load("data/TimeSeriesData.Rda")

classes <- TimeSeriesData %>%
  dplyr::select(c(problem, target)) %>%
  distinct() %>%
  group_by(problem) %>%
  summarise(num_classes = n()) %>%
  ungroup()

# Free up memory as file is large

rm(TimeSeriesData)

# Filter to just problems that finished

classes <- classes %>%
  filter(problem %in% main_models$problem)

#------------
# Scatterplot
#------------

# Join class numbers to classification results and draw plot

p3 <- outputs %>%
  left_join(classes, by = c("problem" = "problem")) %>%
  ggplot(aes(x = num_classes, y = accuracy, colour = method)) +
  geom_jitter(alpha = 0.6) +
  labs(title = "Classification accuracy by number of classes across UCR/UEA repository univariate problems",
       x = "Number of classes",
       y = "Classification accuracy (%)",
       colour = NULL) +
  scale_y_continuous(labels = function(x)paste0(x, "%")) + 
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")

print(p3)
ggsave("output/non-z-scored/complexity-scatter.pdf", p3)

#--------
# Barplot
#--------

# Calculate mean and +- 1 SD for each unique class numbers and plot results

p4 <- outputs %>%
  left_join(classes, by = c("problem" = "problem")) %>%
  group_by(num_classes, method) %>%
  summarise(avg = mean(accuracy, na.rm = TRUE),
            lower = avg - sd(accuracy, na.rm = TRUE),
            upper = avg + sd(accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = num_classes, fill = method)) +
  geom_bar(aes(y = avg), stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "black") +
  labs(title = "Classification accuracy by number of classes across UCR/UEA repository univariate problems",
       subtitle = "Error bars represent +- 1 SD",
       x = "Feature set",
       y = "Classification accuracy (%)",
       fill = NULL) +
  scale_y_continuous(labels = function(x)paste0(x, "%")) + 
  scale_fill_brewer(palette = "Dark2") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  facet_wrap(~method)

print(p4)
ggsave("output/non-z-scored/complexity-bar.pdf", p4)
