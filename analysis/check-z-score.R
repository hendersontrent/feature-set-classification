#------------------------------------------
# This script sets out to check whether all
# problems have been z-scored within machine
# epsilon
#
# NOTE: This script requires setup.R and
# analysis/prepare-time-series-data.R 
# to have been run first
#-----------------------------------------

#-------------------------------------
# Author: Trent Henderson, 2 June 2022
#-------------------------------------

# Load data

load("data/TimeSeriesData.Rda")

#' Function to draw 2-panel plot of mean and SD by problem
#' @param data the dataframe containing time series
#' @param theproblem the problem to draw a plot for
#' @return object of class ggplot2
#' @author Trent Henderson
#' 

check_z_score <- function(data, theproblem){
  
  message(paste0("Doing problem ", match(theproblem, unique(data$problem)), "/", length(unique(data$problem))))
  
  # Filter to problem and compute mean and SD
  
  tmp <- data %>%
    filter(problem == theproblem) %>%
    group_by(id) %>%
    summarise(mu = mean(values, na.rm = TRUE),
              sigma = sd(values, na.rm = TRUE) - 1) %>%
    ungroup() %>%
    pivot_longer(cols = mu:sigma, names_to = "names", values_to = "values") %>%
    mutate(names = ifelse(names == "mu", "Mean", "SD - 1"))
  
  # Draw plot
  
  p <- tmp %>%
    ggplot(aes(x = values)) +
    geom_histogram(aes(y = ..density..), alpha = 0.9, fill = "steelblue2") +
    labs(title = paste0("Distribution of mean and SD - 1 for ", theproblem),
         x = "Value",
         y = "Density",
         caption = "NOTE: Histogram uses 30 bins") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold")) +
    facet_wrap(~names, dir = "v", scales = "free")
  
  print(p)
}

# Run the function for all problems

CairoPDF("output/check-z-score.pdf", 11, 8)

unique(TimeSeriesData$problem) %>%
  purrr::map(~ check_z_score(data = TimeSeriesData, theproblem = .x))

dev.off()
