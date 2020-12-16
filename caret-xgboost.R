SL.caretXGBoost <- function(Y, X, newX, family, obsWeights, id, ...) {
  index <- origami::make_folds(n = length(Y), cluster_ids = id, V = 2)
  index <- lapply(index, function(x) x$training_set)
  control <- caret::trainControl(method = "cv", search = 'random', index = index,
                                 verboseIter = TRUE, classProbs = TRUE)
  SL.caret(Y, X, newX, family, obsWeights, method = 'xgbTree', tuneLength = 5,
           trControl = control, ...)
}
