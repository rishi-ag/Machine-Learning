source("adaBoost.R")
require(reshape2)

#read data
spam <- read.table("spambase.data", header = F, sep = ",")
spam$V58 <- ifelse(spam$V58 == 1, 1,-1)
sub <- sample(nrow(spam), floor(nrow(spam) * 0.7))
train <- spam[sub, ]
test <- spam[-sub, ]

#TRAIN ADA BOOST
depth <- 1
noTree <- 200

train.feat <- train[, 1: ncol(train) - 1]
train.lab <- train[,ncol(train)]
alpha.pred <- rep(0, nrow(train))
w <- rep(1/nrow(train), nrow(train))

train.output <- train.ada.booster(train.feat, train.lab, w, alpha.pred, 
                                  noTree, diagnostics = TRUE, depth)

#TEST ADA BOOST
test.feat <- test[, 1: ncol(test) - 1]
test.lab <- test[,ncol(test)]

test.output <- predict.ada.booster(data = test.feat, labs = test.lab, noTree = noTree, 
                                   alpha.vec = train.output$alpha.vec, depth = depth) 


#PLOT ERRORS
err.df <- data.frame(noTree = 1:noTree, train_error = train.output$error, test_error = test.output$error)
error.melt <- melt(data = err.df, id.vars = 1)
ggplot(error.melt, aes(x = noTree, y = value, col = variable)) + geom_line()
