#' Load and bind all the feature calculations and join in set split labels
#' 
#' @param train_test \code{data.frame} of train-test split labels
#' @param z_scored \code{Boolean} whether to use feature calculations performed on z-scored data or not. Defaults to \code{FALSE}
#' @return \code{data.frame} of bound feature calculations
#' @author Trent Henderson
#' 

bind_all_features <- function(train_test, z_scored = FALSE){
  
  bound_features <- list()
  base_path <- ifelse(z_scored, "data/feature-calcs", "data/feature-calcs/z-scored")
  
  for(i in list.files(base_path, full.names = TRUE, pattern = "\\.Rda")){
    load(i)
    problem_name <- gsub(paste0(base_path, "/"), "\\1", i)
    problem_name <- gsub(".Rda", "\\1", problem_name)
    outs$problem <- problem_name
    bound_features[[i]] <- outs
  }
  
  bound_features <- do.call("rbind", bound_features)
  
  bound_features <- bound_features %>%
    left_join(train_test, by = c("id" = "id", "problem" = "problem"))
  
  return(bound_features)
}
