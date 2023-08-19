#' Load and bind all the feature calculations and join in set split labels
#' 
#' @param path \code{character} denoting file path of the data set to load
#' @return \code{data.frame} of feature calculations
#' @author Trent Henderson
#' 

get_feature_data <- function(path){
  load(path)
  problem_name <- gsub("(.*/\\s*(.*$))", "\\2", path)
  problem_name <- gsub(".Rda", "\\1", problem_name)
  outs$problem <- problem_name
  return(outs)
}
