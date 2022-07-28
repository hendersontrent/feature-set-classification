#------------------------------------------
# This script sets out to produce analysis
# of head-to-head comparisons between all
# individual feature sets
#
# NOTE: This script requires setup.R and
# analysis/compute-features.R and
# analysis/fit-classifiers.R to have been 
# run first
#-----------------------------------------

#--------------------------------------
# Author: Trent Henderson, 28 July 2022
#--------------------------------------

# Load classification results

load("data/outputs.Rda")

outputs <- outputs %>%
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
    group_by(method) %>%
    summarise(mean_acc = mean(balanced_accuracy, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = "method", values_from = "mean_acc")
  
  set1_name <- colnames(tmp2)[1]
  set2_name <- colnames(tmp2)[2]
  
  tmp2 <- tmp2 %>%
    rename(set1 = 1,
           set2 = 2) %>%
    mutate(winner = case_when(
            set1 > set2  ~ set1_name,
            set1 == set2 ~ "tie",
            TRUE         ~ set2_name))
  
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
                       counter = 0,
                       props = 0)
  } else{
    
    # Filter data
    
    tmp <- data %>%
      filter(method %in% c(thesets$set1, thesets$set2))
    
    # Calculate winner for each problem
    
    find_winner_safe <- purrr::possibly(find_winner, otherwise = NULL)
    
    outs <- unique(tmp$problem) %>%
      purrr::map_df(~ find_winner_safe(data = tmp, theproblem = .x)) %>%
      group_by(winner) %>%
      summarise(counter = n()) %>%
      ungroup() %>%
      mutate(props = counter / sum(counter)) %>%
      filter(winner == thesets$set1) %>%
      rename(set1 = winner) %>%
      mutate(set2 = thesets$set2) %>%
      dplyr::select(c(set1, set2, counter, props))
  }
  
  return(outs)
}

# Generate pairwise combinations and map over all of them

combns <- crossing(unique(outputs$method), unique(outputs$method), .name_repair = "unique") %>%
  rename(set1 = 1,
         set2 = 2)

wins <- 1:nrow(combns) %>%
  purrr::map_df(~ calculate_wins(data = outputs, combn_data = combns, rownum = .x))

#---------------------- Draw graphic -----------------------

mypalette <- c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC")

p <- wins %>%
  ggplot(aes(x = set1, y = set2, fill = props)) +
  geom_tile() +
  labs(title = "Head to head of feature sets",
       subtitle = "Mean balanced accuracy was calculated for each problem, with the highest declared winner",
       x = "Feature set",
       y = "Feature set",
       fill = "Proportion of times won") +
  scale_fill_stepsn(n.breaks = 5, colours = rev(mypalette)) +
  theme_bw() +
  theme(legend.position = "bottom")

print(p)
ggsave("output/non-z-scored/head-to-head-matrix.pdf", p)
