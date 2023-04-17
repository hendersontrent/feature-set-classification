#' Fit linear SVM models and evaluate metrics
#' 
#' @param data \code{list} containing train and test sets
#' @param seed \code{integer} denoting fixed value for R's pseudorandom number generator
#' @return \code{data.frame} of classification results
#' @author Trent Henderson
#'

fit_models <- function(data, seed){
  
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
  
  # Fit classifier, generate predictions, and calculate metrics
  
  mod <- e1071::svm(group ~ ., data = train, kernel = "linear", cost = 1, scale = FALSE, probability = TRUE)
  cm <- t(as.matrix(caret::confusionMatrix(predict(mod, newdata = test), test$group)))
  acc <- sum(diag(cm)) / sum(cm)
  bal_acc <- sum(diag(cm)) / (sum(diag(cm)) + (sum(cm) - sum(diag(cm))))
  
  results <- data.frame(model_type = "Main",
                        resample = seed,
                        accuracy = acc,
                        balanced_accuracy = bal_acc)
  
  return(results)
}
