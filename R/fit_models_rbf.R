#' Fit RBF SVM models and evaluate metrics
#' 
#' @param data \code{list} containing train and test sets
#' @param seed \code{integer} denoting fixed value for R's pseudorandom number generator
#' @return \code{data.frame} of classification results
#' @author Trent Henderson
#'

fit_models_rbf <- function(data, seed){
  
  message(paste0("Doing resample ", seed))
  train <- data[[seed]]$Train
  test <- data[[seed]]$Test
  
  #---------------
  # Normalise data
  #---------------
  
  # Get numbers to rescale by from train set
  
  rescalers <- get_rescale_vals(train)
  
  # Apply rescaling
  
  train <- rescale_zscore(train, rescalers)
  test <- rescale_zscore(test, rescalers)
  
  #--------------------------------------
  # Fit classifier, generate predictions 
  # and calculate metrics
  #--------------------------------------
  
  # Regular accuracy and no inverse probability weighting
  
  mod <- e1071::svm(group ~ ., data = train, kernel = "radial", cost = 1, scale = FALSE, probability = TRUE)
  cm <- t(as.matrix(caret::confusionMatrix(predict(mod, newdata = test), test$group)))
  acc <- sum(diag(cm)) / sum(cm)
  
  # Balanced accuracy with inverse probability weighting
  
  class_weights <- 1 / table(train$group)
  mod2 <- e1071::svm(group ~ ., data = train, kernel = "radial", cost = 1, scale = FALSE, probability = TRUE, class.weights = class_weights)
  cm2 <- t(as.matrix(caret::confusionMatrix(predict(mod2, newdata = test), test$group)))
  bal_acc <- sum(diag(cm2) / rowSums(cm2)) / length(diag(cm2) / rowSums(cm2))
  
  # Make final dataframe
  
  results <- data.frame(resample = seed, accuracy = acc, balanced_accuracy = bal_acc)
  return(results)
}
