#' Function to iterate and return top feature results
#' 
#' @param theprob \code{character} denoting the problem name
#' @return object of class \code{data.frame}
#' @author Trent Henderson
#'

iterate_top_features <- function(theprob){
  
  message(paste0("Doing ", theprob))
  load(paste0("data/feature-calcs/z-scored/", theprob ,".Rda"))
  
  the_top <- compute_top_features2(outs_z, 
                                   id_var = "id", 
                                   group_var = "group",
                                   num_features = 40, 
                                   method = "z-score",
                                   test_method = "svmLinear",
                                   use_balanced_accuracy = TRUE,
                                   use_k_fold = TRUE,
                                   num_folds = 10,
                                   use_empirical_null =  TRUE,
                                   null_testing_method = "ModelFreeShuffles",
                                   p_value_method = "gaussian",
                                   num_permutations = 1000,
                                   seed = 123)$ResultsTable %>%
    mutate(problem = theprob)
  
  return(the_top)
}
