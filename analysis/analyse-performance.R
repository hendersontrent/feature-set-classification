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

#' Pull only main results for every problem and feature set
#' @param results the list containing classification results
#' @param x the index of the problem to get results for
#' @returns an object of class dataframe
#' @author Trent Henderson
#' 

pull_main_models <- function(results, x){
  
  # Filter list
  
  tmp <- results[[x]]
  
  # Extract relevant dataframe and filter to main models
  
  tmp <- tmp$RawClassificationResults %>%
    filter(category == "Main") %>%
    mutate(problem = names(results)[[x]])
  
  return(tmp)
}

# Run the function

main_models <- 1:length(outputs) %>%
  purrr::map_df(~ pull_main_models(results = outputs, x = .x))

#------------------ Analysis I: Distribution of accuracies -----------------

# GOAL: Understand distribution of performance across all problems

#--------
# Boxplot
#--------

p <- main_models %>%
  ggplot(aes(x = x, y = accuracy, colour = method)) +
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
ggsave("output/boxplot.pdf", p)

#------------
# Violin plot
#------------

p1 <- main_models %>%
  ggplot(aes(x = x, y = accuracy, colour = method)) +
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
ggsave("output/violin.pdf", p1)

#------------------ Analysis II: Accuracy by problem complexity ------------

# GOAL: Understand performance relative to the "difficulty" of the problem

#-----------------------------------
# Get class numbers for each problem
#-----------------------------------

# Load time series data

load("data/TimeSeriesData.Rda")

classes <- TimeSeriesData %>%
  group_by(problem, target) %>%
  distinct() %>%
  group_by(problem) %>%
  summarise(num_classes = n()) %>%
  ungroup()

# Free up memory as file is large

rm(TimeSeriesData)

#------------
# Scatterplot
#------------

# Join class numbers to classification results and draw plot

p2 <- main_models %>%
  left_join(classes, by = c("problem" = "problem")) %>%
  ggplot(aes(x = num_classes, y = accuracy, colour = method)) +
  geom_point(alpha = 0.6) +
  labs(title = "Classification accuracy by number of classes across UCR/UEA repository univariate problems",
       x = "Number of classes",
       y = "Classification accuracy (%)",
       colour = NULL) +
  scale_y_continuous(labels = function(x)paste0(x, "%")) + 
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")

print(p2)
ggsave("output/complexity-scatter.pdf", p2)

#--------
# Barplot
#--------

# Calculate mean and +- 1 SD for each unique class numbers and plot results

p3 <- main_models %>%
  left_join(classes, by = c("problem" = "problem")) %>%
  group_by(problem, num_classes, method) %>%
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
       colour = NULL) +
  scale_y_continuous(labels = function(x)paste0(x, "%")) + 
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  facet_wrap(~num_classes)

print(p3)
ggsave("output/complexity-bar.pdf", p3)
