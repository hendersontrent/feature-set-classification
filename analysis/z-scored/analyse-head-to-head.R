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
#' @param data the \code{data.frame} to operate on
#' @param theproblem string name of the problem to analyse
#' @param set1name string of the benchmark set to focus on
#' @return object of class \code{data.frame}
#' @author Trent Henderson
#' 

find_winner <- function(data, theproblem, set1name){
  
  tmp2 <- data %>%
    filter(problem == theproblem) %>%
    dplyr::select(c(resample, method, balanced_accuracy)) %>%
    pivot_wider(id_cols = "resample", names_from = "method", values_from = "balanced_accuracy")
  
  if(colnames(tmp2)[2] != set1name){
    tmp2 <- tmp2 %>%
      dplyr::select(c(1, 3, 2))
  }
  
  set1_name <- colnames(tmp2)[2]
  set2_name <- colnames(tmp2)[3]
  fit <- stats::t.test(x = tmp2[, 2], y = tmp2[, 3], data = tmp2)
  
  tmp2 <- data %>%
    filter(problem == theproblem) %>%
    group_by(problem, method) %>%
    summarise(mean_acc = mean(balanced_accuracy, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(id_cols = "problem", names_from = "method", values_from = "mean_acc")
  
  if(colnames(tmp2)[2] != set1name){
    tmp2 <- tmp2 %>%
      dplyr::select(c(1, 3, 2))
  }
  
  tmp2 <- tmp2 %>%
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
#' @param data the \code{data.frame} of classification results
#' @param combn_data the \code{data.frame} of pairwise feature set name combinations
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
                       counter = NA,
                       props = NA,
                       total_probs = NA)
  } else{
    
    # Filter data
    
    tmp <- data %>%
      filter(method %in% c(thesets$set1, thesets$set2))
    
    # Calculate winner for each problem
    
    find_winner_safe <- purrr::possibly(find_winner, otherwise = NULL)
    
    outs <- unique(tmp$problem) %>%
      purrr::map_df(~ find_winner_safe(data = tmp, theproblem = .x, set1name = thesets$set1)) %>%
      dplyr::select(-c(method, mean_accuracy, p_value)) %>%
      distinct() %>%
      group_by(winner) %>%
      summarise(counter = n()) %>%
      ungroup() %>%
      mutate(total_probs = sum(counter),
             props = counter / total_probs) %>%
      filter(winner != "tie") %>%
      rename(set1 = winner) %>%
      mutate(set2 = ifelse(set1 == thesets$set1, thesets$set2, thesets$set1))
  }
  return(outs)
}

# Generate pairwise combinations and map over all of them

combns <- crossing(unique(outputs_z$method), unique(outputs_z$method), .name_repair = "unique") %>%
  rename(set1 = 1,
         set2 = 2)

combns <- combns[!duplicated(data.frame(t(apply(combns, 1, sort)))), ] # Remove duplicates since we get both set's values in the function

wins <- 1:nrow(combns) %>%
  purrr::map_df(~ calculate_wins(data = outputs_z, combn_data = combns, rownum = .x)) %>%
  mutate(label_wins = ifelse(set1 == set2, "-", counter),
         label_total = ifelse(set1 == set2, "-", total_probs)) 

#---------------------- Draw graphic -----------------------

p <- wins %>%
  ggplot(aes(x = set2, y = set1, fill = counter)) +
  geom_tile() +
  geom_text(aes(label = paste0(label_wins, "/", label_total)), colour = "black") +
  labs(title = paste0("Head to head of feature sets over maximum of ", max(wins$total_probs), " problems"),
       subtitle = "t-tests between accuracy distributions for each feature set and problem combination were calculated",
       x = "Test feature set",
       y = "Benchmark feature set",
       fill = "Number of statistical wins") +
  scale_fill_gradient2(low = "#67A9CF",
                       mid = "#F7F7F7",
                       high = "#EF8A62",
                       midpoint = median(wins$counter, na.rm = TRUE),
                       na.value = "grey50") +
  theme_bw() +
  theme(legend.position = "bottom")

print(p)
ggsave("output/z-scored/head-to-head-matrix.pdf", p)
