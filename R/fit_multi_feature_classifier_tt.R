#--------------------------------------------------
# This script sets out to replicate
# the {theft} function fit_multi_feature_classifier
# with modifications for pre-specified
# train-test splits and other resampling things
# for the purposes of this project. It also strips
# back a large amount of functionality in {theft}
# that isn't explicitly needed for this project
# for efficiency, such as null testing and graphing
#--------------------------------------------------

#-------------------------------------
# Author: Trent Henderson, 19 May 2022
#-------------------------------------

#--------------- Helper functions ----------------

#-----------------------
# Classification metrics
#-----------------------

# Recall (for use in computing balanced classification accuracy)

calculate_recall <- function(matrix, x){
  tp <- as.numeric(matrix[x, x])
  fn <- sum(matrix[x, -x])
  
  # Add a catch for when 0s occupy the entire row in the confusion matrix
  # NOTE: Is this the correct way to handle? Seems consistent with {caret}'s default matrix
  
  if(tp + fn == 0){
    recall <- 0
  } else{
    recall <- tp / (tp + fn)
  }
  return(recall)
}

# Four MECE parts of the confusion matrix (TP, FP, TN, FN)

calculate_cm_stats <- function(matrix, x){
  tp <- as.numeric(matrix[x, x])
  fp <- sum(matrix[-x, x])
  tn <- sum(matrix[-x, -x])
  fn <- sum(matrix[x, -x])
  mymat <- matrix(c(tp, fp, tn, fn), nrow = 1, ncol = 4)
  return(mymat)
}

#-------------------------------------
# Calculate balanced accuracy in caret
#-------------------------------------

calculate_balanced_accuracy <- function(data, lev = NULL, model = NULL) {
  
  # Calculate balanced accuracy from confusion matrix as the average of class recalls as per https://arxiv.org/pdf/2008.05756.pdf
  
  cm <- t(as.matrix(caret::confusionMatrix(data$pred, data$obs)$table))
  
  recall <- 1:nrow(cm) %>%
    purrr::map(~ calculate_recall(cm, x = .x)) %>%
    unlist()
  
  balanced_accuracy <- sum(recall) / length(recall)
  
  # Calculate accuracy
  
  accuracy <- sum(diag(cm)) / sum(cm)
  
  # Return results
  
  out <- c(accuracy, balanced_accuracy)
  names(out) <- c("Accuracy", "Balanced_Accuracy")
  return(out)
}

#-----------------------
# Resample model fitting
#-----------------------

#' Function to fit models over 30 resamples as per Bagnall et al. and save results where first sample is always original train-test split
#' @param data the dataframe of feature results to operate on
#' @param train_rows number of cases in the train set
#' @param test_rows number of cases in the test set
#' @param train_groups dataframe containing proportions of each class in original train split
#' @param test_groups dataframe containing proportions of each class in original test split
#' @param x the resample number to control seed for reproducibility
#' @param test_method the algorithm to use for quantifying class separation. Defaults to \code{"gaussprRadial"}
#' @param use_balanced_accuracy a Boolean specifying whether to use balanced accuracy as the summary metric for caret model training. Defaults to \code{FALSE}
#' @param use_k_fold a Boolean specifying whether to use k-fold procedures for generating a distribution of classification accuracy estimates. Defaults to \code{TRUE}
#' @param num_folds an integer specifying the number of folds (train-test splits) to perform if \code{use_k_fold} is set to \code{TRUE}. Defaults to \code{10}
#' @param conf_mat Boolean whether to return confusion matrix instead of dataframe of results. Defaults to \code{FALSE}
#' @returns an object of class dataframe
#' @author Trent Henderson
#' 

fit_resamples <- function(data, train_rows, test_rows, train_groups, test_groups, x, 
                          test_method, use_balanced_accuracy, use_k_fold, num_folds, 
                          conf_mat = FALSE){
  
  message(paste0("Fitting model ", x))
  set.seed(x)
  
  #----------------- Data preparation and resampling ------------------
  
  if(x == 1){
    
    # Set up train and test data
    
    tmp_train <- data %>%
      dplyr::filter(.data$set_split == "Train") %>%
      dplyr::select(-c(.data$set_split)) %>%
      dplyr::select(-c(.data$method)) %>%
      tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
      dplyr::select(-c(.data$id))
    
    tmp_test <- data %>%
      dplyr::filter(.data$set_split == "Test") %>%
      dplyr::select(-c(.data$set_split)) %>%
      dplyr::select(-c(.data$method)) %>%
      tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
      dplyr::select(-c(.data$id))
    
  } else{
    
    # Randomly allocate the correct number of each class to train and test as manual stratified sample
    
    resampled <- list()
    
    for(i in unique(data$group)){
      feasible_ids <- data %>%
        dplyr::filter(group == i) %>%
        dplyr::select(c(id)) %>%
        dplyr::distinct() %>%
        dplyr::pull(id)
      
      n <- train_groups %>%
        dplyr::filter(group == i) %>%
        pull(counter)
      
      traindata <- data.frame(id = sample(feasible_ids, size = n)) %>%
        dplyr::mutate(set_split_new = "Train")
      
      testdata <- data.frame(id = feasible_ids[!feasible_ids %in% traindata$id]) %>%
        dplyr::mutate(set_split_new = "Test")
      
      stopifnot((nrow(traindata) + nrow(testdata)) == length(feasible_ids))
      
      joined <- dplyr::bind_rows(traindata, testdata)
      resampled[[i]] <- joined
    }
    
    resampled <- do.call(rbind, resampled)
    rownames(resampled) <- c()
    
    # Properly set up train and test data
    
    newdata <- data %>%
      dplyr::left_join(resampled, by = c("id" = "id"))
    
    tmp_train <- newdata %>%
      dplyr::filter(.data$set_split_new == "Train") %>%
      dplyr::select(c(.data$id, .data$group, .data$names, .data$values)) %>%
      tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
      dplyr::select(-c(.data$id))
    
    tmp_test <- newdata %>%
      dplyr::filter(.data$set_split_new == "Test") %>%
      dplyr::select(c(.data$id, .data$group, .data$names, .data$values)) %>%
      tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
      dplyr::select(-c(.data$id))
  }
  
  #----------------- Model fitting and returns ------------------
  
  # Fit models
  
  if(use_k_fold){
    
    # Train model
    
    if (use_balanced_accuracy) {
      fitControl <- caret::trainControl(method = "cv",
                                        number = num_folds,
                                        summaryFunction = calculate_balanced_accuracy,
                                        classProbs = TRUE)
    } else {
      fitControl <- caret::trainControl(method = "cv",
                                        number = num_folds,
                                        classProbs = TRUE)
    }
    
    mod <- caret::train(group ~ .,
                        data = tmp_train,
                        method = test_method,
                        trControl = fitControl,
                        preProcess = c("center", "scale", "nzv"))
    
    if(conf_mat){
      mainOuts <- caret::confusionMatrix(predict(mod, newdata = tmp_test), tmp_test$group)
    } else{
      # Get main predictions
      
      u <- dplyr::union(predict(mod, newdata = tmp_test), tmp_test$group)
      mytable <- table(factor(stats::predict(mod, newdata = tmp_test), u), factor(tmp_test$group, u))
      cm <- t(as.matrix(caret::confusionMatrix(mytable)$table))
      
      if(use_balanced_accuracy){
        
        recall <- 1:nrow(cm) %>%
          purrr::map(~ calculate_recall(cm, x = .x)) %>%
          unlist()
        
        balanced_accuracy <- sum(recall) / length(recall)
      }
      
      # Calculate accuracy
      
      accuracy <- sum(diag(cm)) / sum(cm)
      
      if(use_balanced_accuracy){
        mainOuts <- data.frame(accuracy = accuracy, 
                               balanced_accuracy = balanced_accuracy)
      } else{
        mainOuts <- data.frame(accuracy = accuracy)
      }
      
      mainOuts <- mainOuts%>%
        dplyr::mutate(category = "Main")
      
      mainOuts <- mainOuts %>%
        dplyr::mutate(resample = x,
                      num_features_used = (ncol(mod$trainingData) - 1))
    }
    
  } else{
    
    if (use_balanced_accuracy) {
      fitControl <- caret::trainControl(method = "none",
                                        summaryFunction = calculate_balanced_accuracy,
                                        classProbs = TRUE)
    } else {
      fitControl <- caret::trainControl(method = "none",
                                        classProbs = TRUE)
    }
    
    mod <- caret::train(group ~ .,
                        data = tmp_train,
                        method = test_method,
                        trControl = fitControl,
                        preProcess = c("center", "scale", "nzv"))
    
    if(conf_mat){
      mainOuts <- caret::confusionMatrix(predict(mod, newdata = tmp_test), tmp_test$group)
    } else{
      # Get main predictions
      
      u <- dplyr::union(predict(mod, newdata = tmp_test), tmp_test$group)
      mytable <- table(factor(stats::predict(mod, newdata = tmp_test), u), factor(tmp_test$group, u))
      cm <- t(as.matrix(caret::confusionMatrix(mytable)$table))
      
      if(use_balanced_accuracy){
        
        recall <- 1:nrow(cm) %>%
          purrr::map(~ calculate_recall(cm, x = .x)) %>%
          unlist()
        
        balanced_accuracy <- sum(recall) / length(recall)
      }
      
      # Calculate accuracy
      
      accuracy <- sum(diag(cm)) / sum(cm)
      
      if(use_balanced_accuracy){
        mainOuts <- data.frame(accuracy = accuracy, 
                               balanced_accuracy = balanced_accuracy)
      } else{
        mainOuts <- data.frame(accuracy = accuracy)
      }
      mainOuts <- mainOuts
      
      mainOuts <- mainOuts %>%
        dplyr::mutate(resample = x,
                      num_features_used = (ncol(mod$trainingData) - 1))
    }
  }
  return(mainOuts)
}

#--------------
# Model fitting
#--------------

fit_resamples_safe <- purrr::possibly(fit_resamples, otherwise = NULL)

fit_multi_feature_models <- function(data, test_method, use_balanced_accuracy, use_k_fold, num_folds, num_resamples = 30, set = NULL, conf_mat = FALSE){
  
  # Set up input matrices
  
  if(!is.null(set)){
    
    message(paste0("\nCalculating models for ", set))
    
    tmp_mods <- data %>%
      dplyr::filter(.data$method == set)
    
  } else{
    tmp_mods <- data
  }
  
  # Get number of cases in each set
  
  train_rows <- tmp_mods %>%
    dplyr::filter(.data$set_split == "Train") %>%
    dplyr::select(c(.data$id)) %>%
    dplyr::distinct() %>%
    nrow()
  
  test_rows <- tmp_mods %>%
    dplyr::filter(.data$set_split == "Test") %>%
    dplyr::select(c(.data$id)) %>%
    dplyr::distinct() %>%
    nrow()
  
  # Get proportion per class in train and test to use for resample procedure
  
  train_props <- tmp_mods %>%
    dplyr::filter(.data$set_split == "Train") %>%
    dplyr::select(c(.data$id, .data$group)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarise(counter = dplyr::n()) %>%
    dplyr::ungroup()
  
  test_props <- tmp_mods %>%
    dplyr::filter(.data$set_split == "Test") %>%
    dplyr::select(c(.data$id, .data$group)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarise(counter = dplyr::n()) %>%
    dplyr::ungroup()
  
  # Run model fitting via resampling
  
  if(conf_mat){
    finalOuts <- 1:num_resamples %>%
      purrr::map(~ fit_resamples_safe(data = tmp_mods, 
                                      train_rows = train_rows, 
                                      test_rows = test_rows, 
                                      train_groups = train_props, 
                                      test_groups = test_props, 
                                      x = .x, 
                                      test_method = test_method, 
                                      use_balanced_accuracy = use_balanced_accuracy,
                                      use_k_fold = use_k_fold, 
                                      num_folds = num_folds, 
                                      conf_mat = conf_mat))
    
    names(finalOuts) <- paste0("Resample_", 1:num_resamples)
    
  } else{
    finalOuts <- 1:num_resamples %>%
      purrr::map_df(~ fit_resamples_safe(data = tmp_mods, 
                                         train_rows = train_rows, 
                                         test_rows = test_rows, 
                                         train_groups = train_props, 
                                         test_groups = test_props, 
                                         x = .x, 
                                         test_method = test_method, 
                                         use_balanced_accuracy = use_balanced_accuracy,
                                         use_k_fold = use_k_fold, 
                                         num_folds = num_folds, 
                                         conf_mat = conf_mat))
  }
  
  # Return final dataframe
  
  if(!is.null(set) && !conf_mat){
    finalOuts <- finalOuts %>%
      dplyr::mutate(method = set)
  }
  
  return(finalOuts)
}

#---------------
# 'where' helper
#---------------

# NOTE: This is from {tidyselect} but due to import limitations for CRAN (and it not being namespaced) it's rebuilt here
# See https://github.com/r-lib/tidyselect/blob/main/R/helpers-where.R for implementation

mywhere <- function(fn) {
  predicate <- rlang::as_function(fn)
  
  function(x, ...) {
    out <- predicate(x, ...)
    
    if (!rlang::is_bool(out)) {
      rlang::abort("`where()` must be used with functions that return `TRUE` or `FALSE`.")
    }
    
    out
  }
}

#------------------------------
# Pre-processing by feature set
#------------------------------

clean_by_set <- function(data, themethod = NULL){
  
  if(is.null(themethod)){
    tmp_cleaner <- data
    themethod <- "matrix of all features"
  } else{
    tmp_cleaner <- data %>%
      dplyr::filter(.data$method == themethod)
  }
  
  # Widening for model matrix
  
  tmp_cleaner <- tmp_cleaner %>%
    dplyr::mutate(names = paste0(.data$method, "_", .data$names)) %>%
    dplyr::select(-c(.data$method)) %>%
    tidyr::pivot_wider(id_cols = c("id", "group", "set_split"), names_from = "names", values_from = "values")
  
  ncols <- ncol(tmp_cleaner)
  
  # Delete features that contain NAs/Infs and features with constant values
  # Note: We want to delete features rather than observations so the data stays consistent across sets
  
  tmp_cleaner <- tmp_cleaner %>%
    dplyr::select(mywhere(~dplyr::n_distinct(.) > 1))
  
  inds <- apply(tmp_cleaner, 2, function(x)!any(is.na(x)))
  tmp_cleaner <- tmp_cleaner[, inds]
  inds2 <- colSums(tmp_cleaner[4:ncol(tmp_cleaner)])
  inds2 <- inds2[inds2 %ni% c(NA, Inf, -Inf, NaN)]
  tmp_cleaner <- tmp_cleaner[, append(c("id", "group", "set_split"), names(inds2))]
  
  if(ncol(tmp_cleaner) < ncols){
    message(paste0("Dropped ", ncols - ncol(tmp_cleaner), "/", ncol(tmp_cleaner), " features from ", themethod, " due to containing NAs, -Infs/Infs or only a constant."))
  }
  
  # Clean up column (feature) names so models fit properly (mainly an issue with SVM formula) and re-join set labels
  # and prep factor levels as names for {caret} if the 3 base two-class options aren't being used
  
  tmp_cleaner <- tmp_cleaner %>%
    janitor::clean_names() %>%
    tidyr::pivot_longer(cols = 4:ncol(tmp_cleaner), names_to = "names", values_to = "values") %>%
    dplyr::mutate(method = gsub("_.*", "\\1", .data$names)) %>%
    dplyr::mutate(group = as.factor(.data$group)) %>%
    dplyr::mutate(group = as.integer(group),
                  group = paste0("Group_", group),
                  group = make.names(.data$group),
                  group = as.factor(.data$group))
  
  return(tmp_cleaner)
}

#---------------- Main function ----------------

#' Modified version of the \code{theft} function to fit a classifier to feature matrix using all features or all features by set
#' @importFrom rlang .data as_function is_bool abort
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr drop_na pivot_wider pivot_longer
#' @importFrom tibble rownames_to_column
#' @importFrom stats sd reorder ecdf pnorm
#' @importFrom purrr map map_df
#' @importFrom janitor clean_names
#' @importFrom caret preProcess train confusionMatrix
#' @param data the dataframe containing the raw feature data as calculated by \code{theft::calculate_features}
#' @param id_var a string specifying the ID variable to group data on (if one exists). Defaults to \code{"id"}
#' @param group_var a string specifying the grouping variable that the data aggregates to. Defaults to \code{"group"}
#' @param by_set Boolean specifying whether to compute classifiers for each feature set. Defaults to \code{FALSE}
#' @param test_method the algorithm to use for quantifying class separation. Defaults to \code{"gaussprRadial"}
#' @param use_balanced_accuracy a Boolean specifying whether to use balanced accuracy as the summary metric for caret model training. Defaults to \code{FALSE}
#' @param use_k_fold a Boolean specifying whether to use k-fold procedures for generating a distribution of classification accuracy estimates. Defaults to \code{TRUE}
#' @param num_folds an integer specifying the number of folds (train-test splits) to perform if \code{use_k_fold} is set to \code{TRUE}. Defaults to \code{10}
#' @param num_resamples an integer specifying the number of resamples to compute. Defaults to \code{30}
#' @param problem_name the string of the problem to calculate models for
#' @param conf_mat Boolean whether to return confusion matrix instead of dataframe of results. Defaults to \code{FALSE}
#' @return an object of class list containing dataframe summaries of the classification models and a \code{ggplot} object if \code{by_set} is \code{TRUE}
#' @author Trent Henderson
#' 

fit_multi_feature_classifier_tt <- function(data, id_var = "id", group_var = "group",
                                            by_set = FALSE, test_method = "gaussprRadial",
                                            use_balanced_accuracy = FALSE, use_k_fold = TRUE, 
                                            num_folds = 10, num_resamples = 30,
                                            problem_name, conf_mat = FALSE){
  
  #---------- Check arguments ------------
  
  expected_cols_1 <- "names"
  expected_cols_2 <- "values"
  expected_cols_3 <- "method"
  the_cols <- colnames(data)
  '%ni%' <- Negate('%in%')
  
  if(expected_cols_1 %ni% the_cols){
    stop("data should contain at least three columns called 'names', 'values', and 'method'. These are automatically produced by theft::calculate_features(). Please consider running this first and then passing the resultant dataframe to this function.")
  }
  
  if(expected_cols_2 %ni% the_cols){
    stop("data should contain at least three columns called 'names', 'values', and 'method'. These are automatically produced by theft::calculate_features(). Please consider running this first and then passing the resultant dataframe to this function.")
  }
  
  if(expected_cols_3 %ni% the_cols){
    stop("data should contain at least three columns called 'names', 'values', and 'method'. These are automatically produced by theft::calculate_features(). Please consider running this first and then passing the resultant dataframe to this function.")
  }
  
  if(!is.numeric(data$values)){
    stop("'values' column in data should be a numerical vector.")
  }
  
  if(!is.null(id_var) && !is.character(id_var)){
    stop("id_var should be a string specifying a variable in the input data that uniquely identifies each observation.")
  }
  
  # Resamples
  
  if(is.null(num_resamples) || missing(num_resamples)){
    num_resamples <- 30
    message("No argument supplied to num_resamples, using 30 as default.")
  }
  
  #------------- Renaming columns -------------
  
  if (is.null(id_var)){
    stop("Data is not uniquely identifiable. Please add a unique identifier variable.")
  }
  
  if(!is.null(id_var)){
    data_id <- data %>%
      dplyr::rename(id = dplyr::all_of(id_var),
                    group = dplyr::all_of(group_var)) %>%
      dplyr::select(c(.data$id, .data$group, .data$set_split, .data$method, .data$names, .data$values))
  }
  
  num_classes <- length(unique(data_id$group)) # Get number of classes in the data
  
  if(num_classes < 2){
    stop("Your data has less than two unique classes. At least two are required to performed classification analysis.")
  }
  
  # Set defaults for classification method
  
  if((missing(test_method) || is.null(test_method))){
    test_method <- "gaussprRadial"
    message("test_method is NULL or missing, fitting 'gaussprRadial' by default.")
  }
  
  if(length(test_method) != 1){
    stop("test_method should be a single string specification of a classification model available in the `caret` package. 'svmLinear' or 'gaussprRadial' are recommended as starting points.")
  }
  
  # k-fold
  
  if(use_k_fold == TRUE && !is.numeric(num_folds)){
    stop("num_folds should be a positive integer. 10 folds is recommended.")
  }
  
  if(use_k_fold == TRUE && num_folds < 1){
    stop("num_folds should be a positive integer. 10 folds is recommended.")
  }
  
  #------------- Preprocess data --------------
  
  # NOTE: This performs NA checking and filtering by feature set to maximise features and IDs for each if `by_set = TRUE`
  
  if(by_set){
    
    message("Assessing feature values and IDs by individual set.")
    
    data_id <- unique(data_id$method) %>%
      purrr::map_df(~ clean_by_set(data = data_id, themethod = .x))
  } else{
    message("Assessing feature values and IDs using matrix of all features.")
    data_id <- clean_by_set(data = data_id, themethod = NULL)
  }
  
  #------------- Fit models -------------------
  
  #---------------------
  # Set up useful method
  # information
  #---------------------
  
  classifier_name <- test_method
  statistic_name <- ifelse(use_balanced_accuracy, "Mean classification accuracy and balanced classification accuracy", "Mean classification accuracy")
  
  if(by_set){
    
    sets <- unique(data_id$method)
    
    # Compute accuracies for each feature set
    
    if(conf_mat){
      output <- sets %>%
        purrr::map(~ fit_multi_feature_models(data = data_id,
                                              test_method = test_method,
                                              use_balanced_accuracy = use_balanced_accuracy,
                                              use_k_fold = use_k_fold,
                                              num_folds = num_folds,
                                              num_resamples = num_resamples,
                                              set = .x,
                                              conf_mat = conf_mat))
      
      names(output) <- sets
    } else{
      output <- sets %>%
        purrr::map_df(~ fit_multi_feature_models(data = data_id,
                                                 test_method = test_method,
                                                 use_balanced_accuracy = use_balanced_accuracy,
                                                 use_k_fold = use_k_fold,
                                                 num_folds = num_folds,
                                                 num_resamples = num_resamples,
                                                 set = .x,
                                                 conf_mat = conf_mat))
    }
    
  } else{
    
    output <- fit_multi_feature_models(data = data_id,
                                       test_method = test_method,
                                       use_balanced_accuracy = use_balanced_accuracy,
                                       use_k_fold = use_k_fold,
                                       num_folds = num_folds,
                                       num_resamples = num_resamples,
                                       set = NULL,
                                       conf_mat = conf_mat)
  }
  
  #--------------- Return results ---------------
  
  # NOTE: Removed barplot and statistical testing functionality here in {theft} as we don't need it for this project
  # Just shortens the code up since we are doing different comparative testing later on
  
  if(!conf_mat){
  output <- output %>%
      dplyr::mutate(classifier_name = classifier_name,
                    statistic_name = statistic_name,
                    problem = problem_name)
  }
  
  return(output)
}
