#------------------------------------------
# This script sets out to produce analysis
# of head-to-head comparisons between all
# individual feature sets
#
# NOTE: This script requires setup.R and
# analysis/compute-features_z-score.R and
# analysis/fit-classifiers_z-score.R to have 
# been run first
#-----------------------------------------

#----------------------------------------
# Author: Trent Henderson, 08 August 2022
#----------------------------------------

# Load classification results

load("data/outputs_z.Rda")

outputs_z <- outputs_z %>%
  mutate(method = case_when(
    method == "tsfel" ~ "TSFEL",
    method == "kats"  ~ "Kats",
    TRUE              ~ method))

#-------------- Calculate wins for each pairwise combination ------------------

#----------------
# Helper function
#----------------

#' Function to calculate winner for a given problem
#' @param data the dataframe to operate on
#' @param theproblem string name of the problem to analyse
#' @return object of class \code{data.frame}
#' @author Trent Henderson
#' 

find_winner <- function(data, theproblem){
  
  tmp2 <- data %>%
    filter(problem == theproblem) %>%
    dplyr::select(c(resample, method, balanced_accuracy)) %>%
    pivot_wider(id_cols = "resample", names_from = "method", values_from = "balanced_accuracy")
  
  set1_name <- colnames(tmp2)[2]
  set2_name <- colnames(tmp2)[3]
  fit <- stats::t.test(x = tmp2[, 2], y = tmp2[, 3], data = tmp2)
  
  tmp2 <- data %>%
    filter(problem == theproblem) %>%
    group_by(problem, method) %>%
    summarise(mean_acc = mean(balanced_accuracy, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(id_cols = "problem", names_from = "method", values_from = "mean_acc") %>%
    rename(set1 = 2,
           set2 = 3) %>%
    mutate(winner = case_when(
      fit$p.value < .05 & set1 > set2 ~ set1_name,
      fit$p.value < .05 & set2 > set1 ~ set2_name,
      TRUE                            ~ "tie")) %>%
    pivot_longer(cols = set1:set2, names_to = "method", values_to = "mean_accuracy") %>%
    mutate(method = ifelse(method == "set1", set1_name, set2_name),
           p_value = fit$p.value)
  
  return(tmp2)
}

#---------------
# Core operation
#---------------

#' Function to compute pairwise comparison between resamples of accuracy between sets
#' @param data the dataframe of classification results
#' @param combn_data the dataframe of pairwise feature set name combinations
#' @param rownum row number of combination matrix to use
#' @return object of class \code{data.frame}
#' @author Trent Henderson
#' 

calculate_wins <- function(data, combn_data, rownum){
  
  # Filter to correct pairwise combination
  
  thesets <- combn_data[rownum, ]
  message(paste0("Doing: ", thesets$set1, " vs ", thesets$set2))
  
  if(thesets$set1 == thesets$set2){
    outs <- data.frame(set1 = thesets$set1, 
                       set2 = thesets$set2,
                       wins_for_set1 = length(unique(data$problem)),
                       props = 1,
                       total_probs = length(unique(data$problem)))
  } else{
    
    # Filter data
    
    tmp <- data %>%
      filter(method %in% c(thesets$set1, thesets$set2))
    
    # Calculate winner for each problem
    
    find_winner_safe <- purrr::possibly(find_winner, otherwise = NULL)
    
    outs <- unique(tmp$problem) %>%
      purrr::map_df(~ find_winner_safe(data = tmp, theproblem = .x)) %>%
      filter(method == thesets$set1) %>%
      group_by(winner) %>%
      summarise(counter = n()) %>%
      ungroup() %>%
      mutate(total_probs = sum(counter),
             props = counter / total_probs) %>%
      filter(winner == thesets$set1) %>%
      rename(set1 = winner) %>%
      mutate(set2 = thesets$set2) %>%
      dplyr::select(c(set1, set2, counter, total_probs, props)) %>%
      rename(wins_for_set1 = counter)
  }
  
  return(outs)
}

# Generate pairwise combinations and map over all of them

combns <- crossing(unique(outputs_z$method), unique(outputs_z$method), .name_repair = "unique") %>%
  rename(set1 = 1,
         set2 = 2)

wins <- 1:nrow(combns) %>%
  purrr::map_df(~ calculate_wins(data = outputs_z, combn_data = combns, rownum = .x))

#---------------------- Draw graphic -----------------------

p <- wins %>%
  ggplot(aes(x = set1, y = set2, fill = wins_for_set1)) +
  geom_tile() +
  geom_text(aes(label = paste0(wins_for_set1, "/", total_probs)), colour = "white") +
  labs(title = paste0("Head to head of feature sets over maximum of ", max(wins$total_probs), " problems"),
       subtitle = "t-tests between accuracy distributions for each feature set and problem combination were calculated",
       x = "Feature set",
       y = "Feature set",
       fill = "Number of times won") +
  scale_fill_gradient2(low = "white",
                       mid = "#0571B0",
                       high = "#CA0020",
                       midpoint = as.integer(mean(wins$wins_for_set1))) +
  theme_bw() +
  theme(legend.position = "bottom")

print(p)
ggsave("output/z-scored/head-to-head-matrix.pdf", p)
