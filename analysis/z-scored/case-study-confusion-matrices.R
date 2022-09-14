#----------------------------------------------
# This script sets out to analyse the confusion
# matrices for the case study models
#----------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 3 September 2022
#------------------------------------------

# Load in data and summarise to just problem, ID, and train-test set indicator as I didn't bind initially

load("data/TimeSeriesData.Rda")

train_test_ids <- TimeSeriesData %>%
  dplyr::select(c(problem, id, set_split)) %>%
  distinct() %>% 
  filter(problem %in% c("Coffee", "MiddlePhalanxTW", "Plane"))

rm(TimeSeriesData) # Clean up environment as dataframe is large

#---------------- Confusion matrix calculations -----------------

#' Function to map classification performance calculations over datasets/problems
#' @param theproblem filepath to the feature data
#' @param tt_labels the dataframe containing train-test labels
#' @param set Boolean whether to fit by set or not
#' @param set_filt string name of individual feature set to calculate results for. Defaults to \code{NULL} for no usage
#' @returns an object of class dataframe
#' @author Trent Henderson
#' 

get_confusion_matrices <- function(theproblem, tt_labels, set = TRUE, set_filt = NULL){
  
  files <- list.files("data/feature-calcs/z-scored", full.names = TRUE, pattern = "\\.Rda")
  message(paste0("Doing problem ", match(theproblem, files), "/", length(files)))
  load(theproblem)
  problem_name <- gsub(".*/", "\\1", theproblem)
  problem_name <- gsub(".Rda", "\\1", problem_name)
  
  # Join in train-test indicator
  
  outs_z <- outs_z %>%
    filter(names != "DN_Mean") %>% # Just last ditch case in case it slips through
    filter(names != "DN_Spread_Std") %>% # Just last ditch case in case it slips through
    inner_join(tt_labels, by = c("id" = "id")) %>%
    dplyr::select(-c(problem))
  
  if(!is.null(set_filt)){
    outs_z <- outs_z %>%
      filter(method == set_filt)
  }
  
  # Fit multi-feature classifiers by feature set
  
  results <- fit_multi_feature_classifier_tt(outs_z, 
                                             id_var = "id", 
                                             group_var = "group",
                                             by_set = set, 
                                             test_method = "svmLinear", 
                                             use_balanced_accuracy = TRUE,
                                             use_k_fold = TRUE, 
                                             num_folds = 10, 
                                             num_resamples = 1,
                                             problem_name = problem_name,
                                             conf_mat = TRUE)
  
  return(results)
}

data_files <- c("data/feature-calcs/z-scored/Coffee.Rda", "data/feature-calcs/z-scored/MiddlePhalanxTW.Rda",
                "data/feature-calcs/z-scored/Plane.Rda")

conf_mats <- data_files %>%
  purrr::map(~ get_confusion_matrices(theproblem = .x, tt_labels = train_test_ids, set = FALSE))

conf_mats_set <- data_files %>%
  purrr::map(~ get_confusion_matrices(theproblem = .x, tt_labels = train_test_ids, set = TRUE))

#---------------- Confusion matrix analysis -----------------

#-------
# Coffee
#-------

conf_mats[[1]]$Resample_1

#----------------
# MiddlePhalanxTW
#----------------

conf_mats[[2]]$Resample_1

# Each individual confusion matrix for comparison

for(i in 1:length(conf_mats_set[[2]])){
  print(names(conf_mats_set[[2]][i]))
  print(conf_mats_set[[2]][[i]]$Resample_1$overall)
}

conf_mats_set[[2]]$tsfresh$Resample_1

#------
# Plane
#------

conf_mats[[3]]$Resample_1

# Each individual confusion matrix for comparison

conf_mats_set[[3]]$catch22$Resample_1
conf_mats_set[[3]]$feasts$Resample_1
conf_mats_set[[3]]$tsfeatures$Resample_1
conf_mats_set[[3]]$tsfresh$Resample_1
conf_mats_set[[3]]$tsfel$Resample_1
conf_mats_set[[3]]$kats$Resample_1

#---------------- Follow-up pairwise analyses -----------------

#------
# Plane
#------

# Set up binary groups

load("data/feature-calcs/z-scored/Plane.Rda")
plane_binary <- outs_z
rm(outs_z)

plane_binary <- plane_binary %>%
  mutate(group = as.factor(group)) %>%
  mutate(group_1 = ifelse(group == "6", "Group 6", "Everything else"),
         group_2 = ifelse(group == "3", "Group 3", "Everything else")) %>%
  mutate(group_1 = as.factor(group_1),
         group_2 = as.factor(group_2)) %>%
  dplyr::select(-c(group))

# Compute top features

plane_bin_top_1 <- compute_top_features(plane_binary[plane_binary$method == "catch22", ], 
                                        id_var = "id", 
                                        group_var = "group_1",
                                        num_features = 22, 
                                        method = "z-score",
                                        test_method = "svmLinear",
                                        use_balanced_accuracy = TRUE,
                                        use_k_fold = TRUE,
                                        num_folds = 10,
                                        use_empirical_null =  TRUE,
                                        null_testing_method = "ModelFreeShuffles",
                                        p_value_method = "gaussian",
                                        num_permutations = 1e4,
                                        seed = 123)

View(plane_bin_top_1$ResultsTable)

plane_bin_top_2 <- compute_top_features(plane_binary[plane_binary$method == "catch22", ], 
                                        id_var = "id", 
                                        group_var = "group_2",
                                        num_features = 22, 
                                        method = "z-score",
                                        test_method = "svmLinear",
                                        use_balanced_accuracy = TRUE,
                                        use_k_fold = TRUE,
                                        num_folds = 10,
                                        use_empirical_null =  TRUE,
                                        null_testing_method = "ModelFreeShuffles",
                                        p_value_method = "gaussian",
                                        num_permutations = 1e4,
                                        seed = 123)

View(plane_bin_top_2$ResultsTable)

#---------------- Follow-up correlation plots -----------------

#----------------
# MiddlePhalanxTW
#----------------

# Get data for tsfresh

load("data/feature-calcs/z-scored/MiddlePhalanxTW.Rda")
MiddlePhalanxTW <- outs_z %>% filter(method == "tsfresh")
rm(outs_z)

# Draw plot

plot_feature_cors <- function(data, cor_method, clust_method){
  
  # Wrangle dataframe
  
  cor_dat <- data %>%
    dplyr::select(c(.data$id, .data$names, .data$values)) %>%
    tidyr::drop_na() %>%
    tidyr::pivot_wider(id_cols = .data$id, names_from = .data$names, values_from = .data$values) %>%
    dplyr::select(-c(.data$id))
  
  # Delete features that contain NAs/Infs and features with constant values
  # Note: We want to delete features rather than observations so the data stays consistent across sets
  
  cor_dat <- cor_dat %>%
    dplyr::select(mywhere(~dplyr::n_distinct(.) > 1))
  
  inds <- apply(cor_dat, 2, function(x)!any(is.na(x)))
  cor_dat <- cor_dat[, inds]
  inds2 <- colSums(cor_dat[1:ncol(cor_dat)])
  inds2 <- inds2[inds2 %ni% c(NA, Inf, -Inf, NaN)]
  cor_dat <- cor_dat[, names(inds2)]
  
  # Calculate correlations and take absolute
  
  result <- abs(stats::cor(cor_dat, method = cor_method))
  
  # Perform clustering
  
  row.order <- stats::hclust(stats::dist(result, method = "euclidean"), method = clust_method)$order # Hierarchical cluster on rows
  col.order <- stats::hclust(stats::dist(t(result), method = "euclidean"), method = clust_method)$order # Hierarchical cluster on columns
  dat_new <- result[row.order, col.order] # Re-order matrix by cluster outputs
  cluster_out <- reshape2::melt(as.matrix(dat_new)) # Turn into dataframe
  
  # Draw plot
  
  FeatureFeatureCorrelationPlot <- cluster_out %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$Var1, y = .data$Var2)) +
    ggplot2::geom_raster(ggplot2::aes(fill = .data$value)) +
    ggplot2::labs(title = "Pairwise correlation matrix of tsfresh features for MiddlePhalanxTW",
                  x = NULL,
                  y = NULL,
                  fill = "Absolute correlation coefficient") +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_gradient2(low = "#0571B0",
                                  mid = "white",
                                  high = "#CA0020",
                                  midpoint = 0.5) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.position = "bottom",
                   axis.text = ggplot2::element_blank())
  
  return(FeatureFeatureCorrelationPlot)
}

tsfresh_MiddlePhalanxTW <- plot_feature_cors(data = MiddlePhalanxTW, cor_method = "pearson", clust_method = "average")
print(tsfresh_MiddlePhalanxTW)
