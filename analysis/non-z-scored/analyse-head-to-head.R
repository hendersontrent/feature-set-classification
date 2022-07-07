#-------------------------------------
# This script sets out to produce
# head-to-head analyses of performance
# between feature sets
#-------------------------------------

#-------------------------------------
# Author: Trent Henderson, 19 May 2022
#-------------------------------------

# Load classification results

load("data/outputs.Rda")

#----------------- Scatterplots -------------------



#----------------- Lollipop plots -----------------

#' Function to calculate p-values between two sets of accuracies for a problem
#' @param data the dataframe to operate on
#' @param x the problem to compare
#' @param alpha the probability threshold for statistical significant
#' @param correct Boolean of whether to apply a Bonferroni correction or not. Defaults to \code{FALSE}
#' @param num_probs the number of problems to compute in total
#' @returns an object of class dataframe
#' @author Trent Henderson
#' 

calculate_p_value <- function(data, x, alpha = 0.05, correct = FALSE, num_probs){
  
  tmp <- data %>%
    filter(problem == x)
  
  p_vals <- t.test(tmp$set1, tmp$set2, var.eq = FALSE)
  
  if(correct){
    alpha <- alpha / num_probs
  } 
  
  p_vals <- data.frame(problem = x,
                       p_value = p_vals$p.value,
                       significant = ifelse(p_value <= alpha, "Significant Difference", "Non-Significant Difference"))
  
  return(p_vals)
}

#' Function to draw lollipop comparison plots between the sets
#' @param set1 the string of a feature set
#' @param set2 the string of a comparison feature set
#' @param alpha the probability threshold for statistical significant
#' @param correct Boolean of whether to apply a Bonferroni correction or not. Defaults to \code{FALSE}
#' @return an object of class ggplot2
#' @author Trent Henderson
#' 

draw_lollipops <- function(set1, set2, alpha = 0.05, correct = FALSE){
  
  # Define names as new vars as I'll reuse set1 and set2 as column names later
  
  set1_name <- set1
  set2_name <- set2
  
  # Filter to sets of interest and compute differences
  
  p_val_prep <- main_models %>%
    filter(method %in% c(set1, set2)) %>%
    dplyr::select(c(problem, method, accuracy)) %>%
    mutate(accuracy = accuracy * 100) %>%
    pivot_wider(id_cols = c("problem", "resample"), names_from = "method", values_from = "accuracy") %>%
    rename(set1 = 2,
           set2 = 3)
  
  # Calculate p-value for each problem
  
  p_vals <- unique(p_val_prep$problem) %>%
    purrr::map_df(~ calculate_p_value(data = main_models, 
                                      x = .x, 
                                      alpha = 0.05, 
                                      correct = FALSE, 
                                      num_probs = length(unique(p$problem))))
  
  p <- main_models %>%
    group_by(problem, method) %>%
    summarise(accuracy = mean(accuracy, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(id_cols = "problem", names_from = "method", values_from = "accuracy") %>%
    mutate(difference = set1 - set2,
           winner = ifelse(difference > 0, paste0(set1_name, " better"), paste0(set2_name, " better"))) %>%
    left_join(p_vals, by = c("problem" = "problem"))
  
  # Table summary for subtitle
  
  results <- as.data.frame(table(p$winner)) %>%
    arrange(-Freq) %>%
    mutate(Var1 = gsub(" .*", "\\1", Var1),
           big_string = paste0(Var1, " wins ", Freq, "/", sum(Freq), " (", (round(Freq / sum(Freq), digits = 2) * 100) ,"%)"))
  
  # Draw plot
  
  mypal <- c("Non-Significant Difference" = "grey50",
             "Significant Difference" = "#1B9E77")
  
  p <- p %>%
    ggplot(aes(x = problem, y = difference, colour = significant)) +
    annotate(geom = "rect", xmin = -Inf, xmax = Inf, ymax = 60, ymin = 0,
             fill = "steelblue2", alpha = 0.2) +
    scale_x_discrete() +
    annotate("text", x = 10, y = -50, label = paste0(set2, " better")) +
    annotate("text", x = 10, y = 50, label = paste0(set1, " better")) +
    geom_segment(aes(x = problem, xend = problem, y = 0, yend = difference)) +
    geom_point(size = 2) +
    labs(title = paste0("Comparative accuracy between ", set1_name, " and ", set2_name),
         subtitle = paste0(results$big_string[1], ", ", results$big_string[2]),
         x = "Problem",
         y = "Classification accuracy (%) difference",
         colour = NULL) +
    scale_colour_manual(values = mypal) +
    scale_y_continuous(limits = c(-60, 60),
                       breaks = seq(from = -60, to = 60, by = 20),
                       labels = function(x)paste(x, "%")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90),
          panel.grid.minor = element_blank(),
          legend.position = "bottom")
  
  ggsave(paste0("output/non-z-scored/lollipop_", set1, "_vs_", set2, ".pdf"), p)
}

# Get all pairwise combinations to map over

combns <- crossing(unique(main_models$method), unique(main_models$method),
                   .name_repair = "minimal") %>%
  rename(set1 = 1,
         set2 = 2) %>%
  filter(set1 != set2) %>% # Remove self-comparisons
  rowwise() %>% 
  mutate(alphabetical = paste(sort(unlist(strsplit(paste(set1, set2, sep = " "), " "))), collapse = " ")) %>% # Remove duplicates due to ordering
  distinct(alphabetical) %>%
  mutate(set1 = gsub(" .*", "\\1", alphabetical),
         set2 = gsub(".* ", "\\1", alphabetical)) %>% # Clean up to 2 columns to map over
  dplyr::select(-c(alphabetical))

# Draw plots

purrr::map2(combns$set1, combns$set2, draw_lollipops)
