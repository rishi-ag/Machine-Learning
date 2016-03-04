if(!require("assertthat")) install.packages("assertthat")
if(!require("rpart")) install.packages("rpart")
if(!require("psych")) install.packages("psych")
if(!require("dplyr")) install.packages("dplyr")
if(!require("mvtnorm")) install.packages("mvtnorm")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("randomForest")) install.packages("randomForest")
if(!require("reshape2")) install.packages("reshape2")
if(!require("adabag")) install.packages("adabag")

get.sigmaXY <- function(rhoXY, sdX, sdY){
  cov <- rhoXY * sdX *sdY
  matrix(c(sdX^2, cov, cov, sdY^2), nrow = 2)
}

get.data <- function(n1 = 100, n2 = 100, mu1, mu2, sd1, sd2, rhoXY, seed = 1000) {
  set.seed(seed)
  features1 <- rmvnorm(n1, mean = c(mu1[1], mu1[2]), 
                       sigma = get.sigmaXY(rhoXY[1], sd1[1], sd1[2]))
  features2 <- rmvnorm(n2, mean = c(mu2[1], mu2[2]), 
                       sigma = get.sigmaXY(rhoXY[2], sd2[1], sd2[2]))
  features <- rbind(features1, features2)
  label <- c(rep(-1, n1), rep(1, n2))
  index <- sample(1:(n1+n2))
  result <- list(features = as.data.frame(rbind(features1, features2))[index,], 
                 label = label[index])
  return(result)
}


draw.plot <- function(data, lab) {
  data <- data.frame(data, factor(lab))
  names(data) <- c("X1", "X2", "lab")
  ggplot(data, aes(x = X1, y = X2, fill = lab, color = lab)) +
    geom_point(alpha = 0.5, size = 3) +
    scale_color_manual(values = c("#AD0909", "#202D8F"))
  
}


ada.booster <- function(data, lab, w, alpha.pred, noTree = 10, 
                        diagnostics = FALSE, err.rate = c(), depth = 1) {
  #check inputs
  assert_that(length(lab) == nrow(data))
  assert_that(length(lab) == length(w))
  assert_that(length(lab) == length(alpha.pred))
  
  
  tree = rpart(formula = lab~., data = data, 
               weights = w, method = "class", control = c(maxdepth = depth) )
  pred = ifelse(predict(tree, type = "class") == "-1", -1, 1)
  
  epsilon = (function() sum(w * ifelse(pred == lab, 0, 1))) ()
  alpha   = (function(x) 0.5 * log((1 - x) / x)) (epsilon)
  w       = w * exp((alpha * ifelse(pred != lab, 1, -1)))
  
  alpha.pred = alpha.pred + ifelse(pred == "-1", -1, 1) * alpha
  
  if(diagnostics == TRUE) err.rate <- c(err.rate, 
                                        sum(sign(alpha.pred) != lab) / length(lab)) 
  
  #Check if all weights are 0 i.e data is prefectly seperated
  zero.weights = (max(w) == 0 & min (w) == 0)
  
  if(noTree == 1 | zero.weights) {
    if(zero.weights) print("Data is prefectly seperated. Quitting")
    
    if(diagnostics != TRUE) return(sign(alpha.pred))
    else return (err.rate)
  }
  else {
    ada.booster(data, lab, w, alpha.pred, noTree - 1, diagnostics, err.rate, depth)
  }
}

#Create dataset
rhoXY <- c(0.8, 0.5)
sd1 <- c(2, 2)
mu1 <- c(15, 20)
sd2 <- c(1, 1.5)
mu2 <- c(15, 15)

train <- get.data(1000,1000, mu1, mu2, sd1, sd2, rhoXY, 2500)
test <- get.data(500,500, mu1, mu2, sd1, sd2, rhoXY, 1200)

train.data <- train[[1]]
train.lab <- train[[2]]
#train.lab <- train.lab[1:(length(train.lab) - 1)]
test.data <- test[[1]]
test.lab <- test[[2]]

draw.plot(train.data, train.lab)
draw.plot(test.data, test.lab)

noTree <- 100

#ADA BOOST
alpha.pred <- rep(0, length(train.lab))
w <- rep(1/length(train.lab), length(train.lab))
ad.err.rates <- ada.booster(train.data, train.lab, w, alpha.pred, noTree, diagnostics = TRUE, depth)

err.df <- data.frame(noTree = 1:noTree, Ada_Boost_Error = ad.err.rates)
ggplot(err.df, aes(x = noTree, y = Ada_Boost_Error)) + geom_point() + geom_line()



#Compare
alpha.pred <- rep(0, length(train.lab))
w <- rep(1/length(test.lab), length(test.lab))
ad.err.rates <- ada.booster(test.data, test.lab, w, alpha.pred, noTree, diagnostics = TRUE)


rf.err.rates <- sapply(X = 1:noTree, FUN = function(v) {
  rf = randomForest(train.data, train.lab, ntree = v)
  pred = sign(predict(rf , test.data, type = "class"))
  sum(pred != test.lab) / length(test.lab)
})

bag.train <- cbind(lab = factor(train.lab), train.data)
bag.test <- cbind(lab = factor(test.lab), test.data)
names(bag.train)
names(bag.test)


bag.err.rates <- sapply(X = 1:noTree, FUN = function(v) {
  bag = bagging(lab ~., bag.train, mfinal=v)
  err = predict.bagging(bag, bag.test)$error
  err
})

err.df <- data.frame(noTree = 1:noTree, ada.boost = ad.err.rates, 
                     rand.forest =rf.err.rates, bagging = bag.err.rates)
melt.error <- melt(err.df, id.vars = "noTree", variable.name = "Algorithm", 
                   value.name = "Error")
names(melt.error)
ggplot(melt.error, aes(x = noTree, y = Error, color = Algorithm)) + 
  geom_point() + 
  geom_line() +
  scale_color_manual(values = c("#1f78b4", "#33a02c", "#ff7f00")) +
  ggtitle("Error Comparison")