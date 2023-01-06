#' Sample IDs by class
#' @param data the dataframe containing time-series data
#' @param group_name string specifying the class to filter by
#' @param n number of samples to generate
#' @return object of class vector
#' @author Trent Henderson
#' 

draw_samples <- function(data, group_name, n){
  
  samps <- data %>%
    filter(target == group_name) %>%
    dplyr::select(id) %>%
    distinct() %>%
    pull(id) %>%
    sample(size = n)
  
  return(samps)
}

#' Draw time series plots by class
#' @param data the dataframe containing time series data
#' @param n number of samples to take
#' @param seed fix R's pseudo-random number generator
#' @author Trent Henderson
#'

plot_samples <- function(data, n = 2, seed = 123){
  
  set.seed(seed)
  
  # Generate IDs to sample
  
  ids <- unique(data$target) %>%
    purrr::map(~ draw_samples(data = data, group_name = .x, n = n)) %>%
    unlist()
  
  # Draw plot
  
  p <- data %>%
    filter(id %in% ids) %>%
    mutate(id = factor(id, levels = ids)) %>%
    ggplot(aes(x = timepoint, y = values, colour = target)) +
    geom_line() +
    labs(title = paste0("Random sample of ", n, " time series from each class for ", unique(data$problem)),
         x = "Time",
         y = "Value",
         colour = "Class") +
    theme_bw() +
    theme(legend.position = "bottom",
          strip.background = element_blank(),
          strip.text = element_text(face = "bold")) +
    facet_wrap(~id, ncol = n, nrow = length(unique(data$target)))
  
  return(p)
}

#' Draw all time series faceted by class with a mean line
#' @param data the dataframe containing time series data
#' @param colour_by_split Boolean whether to colour each line by its Train/Test allocation. Defaults to \code{TRUE}
#' @author Trent Henderson
#'

plot_all_ts <- function(data, colour_by_split = TRUE){
  
  # Calculate mean
  
  mu <- data %>%
    mutate(target = as.factor(target)) %>%
    group_by(timepoint, target) %>%
    summarise(mu = mean(values, na.rm = TRUE)) %>%
    ungroup()
  
  # Draw plot
  
  p <- data %>%
    mutate(id = as.factor(id),
           target = as.factor(target))
  
  p <- p %>%
    ggplot(aes(x = timepoint, y = values))
  
  if(colour_by_split){
    p <- p +
      geom_line(aes(group = id, colour = set_split), size = 0.6, alpha = 0.8)
  } else{
    p <- p +
      geom_line(aes(group = id), colour = "grey50", size = 0.6, alpha = 0.8)
  }
  
  p <- p +
    geom_line(data = mu, aes(x = timepoint, y = mu), size = 1, colour = mypal[1]) +
    labs(title = paste0("Time series plots for each class for ", unique(data$problem)),
         subtitle = "Mean is represented by thick red line.",
         x = "Time",
         y = "Value",
         colour = NULL) +
    theme_bw() +
    theme(legend.position = "bottom",
          strip.background = element_blank(),
          strip.text = element_text(face = "bold")) +
    facet_wrap(~target, ncol = 1, nrow = length(unique(mu$target)))
  
  return(p)
}
