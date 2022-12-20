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
#' @param problem_data the dataframe contain problem summary information
#' @return object of class \code{data.frame}
#' @author Trent Henderson
#' 

find_winner <- function(data, theproblem, set1name, problem_data){
  
  tmp2 <- data %>%
    filter(problem == theproblem) %>%
    dplyr::select(c(resample, method, balanced_accuracy)) %>%
    pivot_wider(id_cols = "resample", names_from = "method", values_from = "balanced_accuracy")
  
  if(colnames(tmp2)[2] != set1name){
    tmp2 <- tmp2 %>%
      dplyr::select(c(1, 3, 2))
  }
  
  # Filter to get parameters for correlated t-test
  
  params <- problem_data %>%
    filter(problem == theproblem)
  
  # Set up vectors
  
  x <- as.vector(unlist(tmp2[, 2]))
  y <- as.vector(unlist(tmp2[, 3]))
  
  # Do calcs
  
  set1_name <- colnames(tmp2)[2]
  set2_name <- colnames(tmp2)[3]
  t_test <- corr_t_test(x = x, y = y, n = 30, n1 = as.integer(params$Train), n2 = as.integer(params$Test))
  
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
    mutate(p.value = as.numeric(t_test$p.value))
  
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
                       total_probs = NA,
                       ties = NA)
  } else{
    
    # Filter data
    
    tmp <- data %>%
      filter(method %in% c(thesets$set1, thesets$set2))
    
    # Calculate winner for each problem
    
    find_winner_safe <- purrr::possibly(find_winner, otherwise = NULL)
    
    outs <- unique(tmp$problem) %>%
      purrr::map_df(~ find_winner_safe(data = tmp, theproblem = .x, set1name = thesets$set1, problem_data = problem_summaries)) %>%
      rename(set1 = 2,
             set2 = 3) %>%
      mutate(p.value.adj = p.adjust(p.value, method = "holm"),
             winner = case_when(
               p.value.adj < .05 & set1 > set2 ~ thesets$set1,
               p.value.adj < .05 & set2 > set1 ~ thesets$set2,
               TRUE                            ~ "tie")) %>%
      group_by(winner) %>%
      summarise(counter = n()) %>%
      ungroup() %>%
      mutate(total_probs = sum(counter),
             props = counter / total_probs)
    
    ties <- outs %>%
      filter(winner == "tie")
    
    outs <- outs %>%
      filter(winner != "tie") %>%
      rename(set1 = winner) %>%
      mutate(set2 = ifelse(set1 == thesets$set1, thesets$set2, thesets$set1),
             ties = ties$counter) %>%
      filter(set1 != thesets$set1) # For upper triangular glory
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
  mutate(my_lab = ifelse(set1 == set2, "-", paste0(counter, "-", total_probs - (counter + ties)))) %>%
  dplyr::select(-c(props))

# Invert dataframe to get lower triangle of matrix graphic

losses <- wins %>%
  mutate(set3 = set2,
         set4 = set1) %>%
  dplyr::select(-c(set1, set2, my_lab)) %>%
  rename(set1 = set3,
         set2 = set4) %>%
  mutate(my_lab = ifelse(set1 == set2, "-", paste0(total_probs - (counter + ties), "-", counter)),
         counter = total_probs - (counter + ties)) %>%
  dplyr::select(c(set1, set2, counter, my_lab))

# Row bind both

both <- wins %>%
  dplyr::select(c(set1, set2, counter, my_lab)) %>%
  bind_rows(losses) %>%
  group_by(set1) %>%
  mutate(total_wins = sum(counter, na.rm = TRUE)) %>%
  ungroup()

#---------------------- Draw graphic -----------------------

p <- both %>%
  ggplot(aes(x = reorder(set2, -total_wins), y = reorder(set1, -total_wins), fill = counter)) +
  geom_tile() +
  geom_text(aes(label = my_lab), colour = "black", size = 5) +
  labs(x = "Comparison feature set",
       y = "Feature set (W-L)",
       fill = "Number of statistical wins") +
  scale_fill_gradient(low = "white", high = "#FF0029", na.value = "grey50",
                      limits = c(0, 10),
                      breaks = c(0, 2, 4, 6, 8, 10),
                      labels = c(0, 2, 4, 6, 8, 10)) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11))

print(p)
ggsave("output/z-scored/head-to-head-matrix.pdf", p, units = "in", height = 9, width = 9)
