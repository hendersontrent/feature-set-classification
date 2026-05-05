#---------------------------------------
# This script calculates pairwise
# statistical comparisons between feature
# sets across the UEA/UCR Repository
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 4 May 2026
#---------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(correctR)

#--------------- Define functions ---------------

#' Get train-test sample sizes for each problem
#' 
#' @return \code{data.frame} of problem summaries
#' @author Trent Henderson
#' 

get_n <- function(){
  
  n_storage <- vector(mode = "list", length = length(list.files("feature-calculations/train-test-labels/")))
  
  for(i in 1:length(n_storage)){
    rm(label)
    load(paste0("feature-calculations/train-test-labels/", list.files("feature-calculations/train-test-labels/")[i]))
    
    n_tmp <- label |>
      distinct() |>
      reframe(counter = n(), .by = c("problem", "train_test")) |>
      pivot_wider(id_cols = "problem", names_from = "train_test", values_from = "counter")
    
    n_storage[[i]] <- n_tmp
  }
  
  n_storage <- do.call("rbind", n_storage)
  return(n_storage)
}

#' Calculate winner for a given problem
#' 
#' @param data \code{data.frame} to operate on
#' @param theproblem \code{character} denoting the dataset to work on
#' @param set1name \code{character} denoting the benchmark set to focus on
#' @param problem_data \code{data.frame} containing problem summary information
#' @return object of class \code{data.frame}
#' @author Trent Henderson
#' 

find_winner <- function(data, theproblem, set1name, problem_data){
  
  tmp2 <- data |>
    filter(problem == theproblem) |>
    dplyr::select(c(resample, feature_set, accuracy)) |>
    pivot_wider(id_cols = "resample", names_from = "feature_set", values_from = "accuracy")
  
  if(is.na(colSums(tmp2[, 2])) | is.na(colSums(tmp2[, 3]))){
  } else{
    
    if(colnames(tmp2)[2] != set1name){
      tmp2 <- tmp2 |>
        dplyr::select(c(1, 3, 2))
    }
    
    # Filter to get parameters for correlated t-test
    
    params <- problem_data |>
      filter(problem == theproblem)
    
    # Set up vectors
    
    x <- as.vector(unlist(tmp2[, 2]))
    y <- as.vector(unlist(tmp2[, 3]))
    
    # Do calcs
    
    set1_name <- colnames(tmp2)[2]
    set2_name <- colnames(tmp2)[3]
    
    t_test <- correctR::resampled_ttest(x = x, y = y, n = 100, 
                                        n1 = as.integer(params$Train), n2 = as.integer(params$Test),
                                        tailed = "two")
    
    tmp2 <- data |>
      filter(problem == theproblem) |>
      reframe(mean_acc = mean(accuracy, na.rm = TRUE),
              .by = c("problem", "feature_set")) |>
      pivot_wider(id_cols = "problem", names_from = "feature_set", values_from = "mean_acc")
    
    if(colnames(tmp2)[2] != set1name){
      tmp2 <- tmp2 |>
        dplyr::select(c(1, 3, 2))
    }
    
    tmp2 <- tmp2 |>
      mutate(p.value = as.numeric(t_test$p.value))
    
    return(tmp2)
  }
}

#---------------
# Core operation
#---------------

#' Compute pairwise comparison between resamples of accuracy between sets
#' 
#' @param data \code{data.frame} of classification results
#' @param combn_data \code{data.frame} of pairwise feature set name combinations
#' @param rownum \code{integer} denoting the row number of the combination data to use
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
    
    tmp <- data |>
      filter(feature_set %in% c(thesets$set1, thesets$set2))
    
    # Calculate winner for each problem
    
    outs <- unique(tmp$problem) |>
      purrr::map_df(~ find_winner(data = tmp, theproblem = .x, set1name = thesets$set1, problem_data = problem_summaries)) |>
      rename(set1 = 2,
             set2 = 3) |>
      mutate(p.value.adj = p.adjust(p.value, method = "holm"),
             winner = case_when(
               p.value < .05 & set1 > set2 ~ thesets$set1,
               p.value < .05 & set2 > set1 ~ thesets$set2,
               TRUE                        ~ "tie")) |>
      reframe(counter = n(), .by = "winner") |>
      mutate(total_probs = sum(counter),
             props = counter / total_probs)
    
    ties <- outs |>
      filter(winner == "tie")
    
    outs <- outs |>
      filter(winner != "tie") |>
      rename(set1 = winner) |>
      mutate(set2 = ifelse(set1 == thesets$set1, thesets$set2, thesets$set1),
             ties = ties$counter) |>
      filter(set1 != thesets$set1) # For upper triangular glory
  }
  return(outs)
}

#' Calculate pairwise comparisons and draw summary graphic
#' 
#' @param model_type \code{character} denoting the type of model to fit. Can be one of \code{"glmnet"} or \code{"svm"}
#' @return \code{ggplot} containing the summary graphic
#' @author Trent Henderson
#' 

h2h <- function(model_type = c("glmnet", "svm")){
  
  model_type <- match.arg(model_type)

  # Pull data

  files <- list.files(paste0("classification-models/results/", model_type, "/"))
  results <- vector(mode = "list", length = length(files))

  for(i in 1:length(results)){
    results[[i]] <- read.csv(paste0("classification-models/results/", model_type, "/", files[i]))
  }
  
  results <- do.call("rbind", results) |>
    filter(feature_set != "timegp")
  
  # Generate pairwise combinations and map over all of them
  
  combns <- crossing(unique(results$feature_set), unique(results$feature_set), .name_repair = "unique") |>
    rename(set1 = 1, set2 = 2)
  
  combns <- combns[!duplicated(data.frame(t(apply(combns, 1, sort)))), ] # Remove duplicates since we get both set's values in the function
  
  wins <- 1:nrow(combns) |>
    purrr::map_df(~ calculate_wins(data = results, combn_data = combns, rownum = .x)) |>
    mutate(my_lab = ifelse(set1 == set2, "-", paste0(counter, "-", total_probs - (counter + ties)))) |>
    dplyr::select(-c(props))
  
  # Invert dataframe to get lower triangle of matrix graphic
  
  losses <- wins |>
    mutate(set3 = set2,
           set4 = set1) |>
    dplyr::select(-c(set1, set2, my_lab)) |>
    rename(set1 = set3,
           set2 = set4) |>
    mutate(my_lab = ifelse(set1 == set2, "-", paste0(total_probs - (counter + ties), "-", counter)),
           counter = total_probs - (counter + ties)) |>
    dplyr::select(c(set1, set2, counter, my_lab))
  
  # Row bind both
  
  both <- wins |>
    #dplyr::select(c(set1, set2, counter, my_lab)) |>
    bind_rows(losses) |>
    group_by(set1) |>
    mutate(total_wins = sum(counter, na.rm = TRUE)) |>
    ungroup()
  
  # Draw plot
  
  p <- both |>
    ggplot(aes(x = reorder(set2, -total_wins), y = reorder(set1, -total_wins), fill = counter)) +
    geom_tile() +
    geom_text(aes(label = my_lab), colour = "black", size = 5) +
    labs(x = "Comparison feature set",
         y = "Feature set (W-L)",
         fill = "Number of wins")
  
  if(model_type == "svm"){
    p <- p +
      scale_fill_gradient(low = "white", high = "#CA0020", na.value = "grey85",
                          limits = c(0, 50),
                          breaks = seq(from = 0, to = 50, by = 10),
                          labels = seq(from = 0, to = 50, by = 10))
  } else{
    p <- p +
      scale_fill_gradient(low = "white", high = "#CA0020", na.value = "grey85",
                          limits = c(0, 45),
                          breaks = seq(from = 0, to = 50, by = 10),
                          labels = seq(from = 0, to = 50, by = 10))
  }
  
p <- p +
    theme_bw() +
    theme(legend.position = "bottom",
          panel.grid = element_blank(),
          axis.text = element_text(colour = "black"),
          legend.key.size = unit(1, "cm"))
  
  return(p)
}

#--------------- Run the function for all models ---------------

# Get problem summaries

problem_summaries <- get_n()

# Run comparisons

p_svm <- h2h(model_type = "svm")
print(p_svm)
ggsave("output/head-to-head-matrix.pdf", p_svm, units = "in", height = 9, width = 9)
