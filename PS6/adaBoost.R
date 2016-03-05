if(!require("assertthat")) install.packages("assertthat")
if(!require("rpart")) install.packages("rpart")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("reshape2")) install.packages("reshape2")

train.ada.booster <- function(data, lab, w, alpha.pred, noTree = 10, 
                        diagnostics = FALSE, err.rate = c(), depth = 1, alpha.vec = c()) {
  #' function is recusrive
  #' w is initial weights
  #' alpha.pred is the predictions at veery iteration
  #' diagnostics = TRUE returns final error rate for every iteration and alpha
  #' diagnostics = FALSE returns final predictions and alpha
  #' alpha.vec is the alpha at every iteration. it is the main output of the training
  #' 
  tree = rpart(formula = lab~., data = data, 
               weights = w, method = "class", control = c(maxdepth = depth) )
  pred = ifelse(predict(tree, type = "class") == "-1", -1, 1)
  
  epsilon = (function() sum(w * ifelse(pred == lab, 0, 1))) ()
  alpha   = (function(x) 0.5 * log((1 - x) / x)) (epsilon)
  w       = w * exp((alpha * ifelse(pred != lab, 1, -1)))
  
  alpha.vec = c(alpha.vec, alpha)
  alpha.pred = alpha.pred + ifelse(pred == "-1", -1, 1) * alpha
  
  
  if(diagnostics == TRUE) err.rate <- c(err.rate, 
                                        sum(sign(alpha.pred) != lab) / length(lab)) 
  
  #Check if all weights are 0 i.e data is prefectly seperated
  zero.weights = (max(w) == 0 & min (w) == 0)
  
  if(noTree == 2 | zero.weights) {
    if(zero.weights) print("Data is prefectly seperated. Quitting")
    
    if(diagnostics != TRUE) {
      return(list(prediction = sign(alpha.pred), weights = w, alpha.vec = alpha.vec))
    } else {
      return (list(error = err.rate, alpha.vec = alpha.vec))
    }
  }
  else {
    train.ada.booster(data, lab, w, alpha.pred, noTree - 1, diagnostics, err.rate, depth, alpha.vec)
  }
}

predict.ada.booster <- function(data, labs, noTree, alpha.vec, depth, save.result = TRUE) {
  weights = rep(1/nrow(data), nrow(data))
  
  alpha.pred = rep(0, nrow(data))
  prediction = c()
  err.rate = c()
  
  #i = 1
  for(i in 1:noTree){
    tree = rpart(formula = labs~., data = data, 
                 weights = weights, method = "class", control = c(maxdepth = depth) )
    
    pred = ifelse(predict(tree, type = "class") == "-1", -1, 1)
    
    epsilon = (function() sum(weights * ifelse(pred == labs, 0, 1))) ()
    weights = weights * exp((alpha.vec[i] * ifelse(pred != labs, 1, -1)))
    
    alpha.pred = alpha.pred + ifelse(pred == "-1", -1, 1) * alpha.vec[i]
    prediction = sign(alpha.pred)
    err.rate = c(err.rate, sum(labs != prediction)/length(labs))
  }
  
  if(save.result) write.csv(x = prediction, file = "prediction.csv")
  return(list(predLabels = prediction, error = err.rate))
}