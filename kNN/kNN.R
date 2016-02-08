library("assertthat")
library("mvtnorm")
library("reshape2")
library("doParallel")

source("./genData.R")
#--------------------------------------------------------------------------------
CalcDist <- function(row, data, p = 2) {
    #' calculate distance between a row and given dataset
    
    row.matrix <- matrix( rep(as.numeric(row), each = nrow(data)) , nrow = nrow(data))
    
    return((rowSums( abs((data - row.matrix))^p )) ^ (1.0/p))
}

#--------------------------------------------------------------------------------
LabelCount <- function(vec, class.labels){
    freq <- vector()
    for(class in class.labels){
        freq <- c(freq, sum(vec == class))
    }
    return(freq)
}
#--------------------------------------------------------------------------------

DistMatrix <- function(features, memory = NULL, p, mode = "train"){
    #' Creates a matrix of distances of training data set and new dataset depending on 
    #' value of variable type
    #' mode can be "train" or "test"
    
    if(mode == "train"){
        dist.matrix <- apply(features, 1, function(row) CalcDist(row, features, p))
    } else if(mode == "test") {
        dist.matrix <- apply( features, 1, function(row) CalcDist(row, memory, p))
    }
    
    return(dist.matrix)
}


OrderMatrix <- function(dist.matrix, mode = "train", k, labels){
    #' returns ordered matrix of classes cut off at specified
    #' value of k
    
    
    if(mode == "train") {
        order.matrix <- as.matrix(t(apply(dist.matrix, 1, order)))
        order.matrix <- order.matrix[,2:(k+1), drop = FALSE]
    } else if(mode == "test") {
        order.matrix <- as.matrix(t(apply(dist.matrix, 2, order)))
        order.matrix <- order.matrix[,1:k, drop = FALSE]
    }
    
    class.matrix <- matrix(0, nrow = nrow(order.matrix), ncol = ncol(order.matrix))
    
    for(row in 1:nrow(order.matrix)) {
        for(col in 1:ncol(order.matrix)) {
            class.matrix[row, col] <- labels[order.matrix[row, col]]
        }
    }
    
    return(class.matrix)
}

#Calculates order matrix (neighbors) for Training data
Train <- function(features, labels, class.labels, k = 1, p = 2) {
    #Calculates order matrix (neighbors) for Training data
    
    #create dist matrix
    dist.matrix <- DistMatrix(features = features, p = p, mode = "train")
    
    #order.matrix
    order.matrix <- OrderMatrix(dist.matrix, mode = "train", k = k, labels = labels)
    
    
    #calculate class probabilities
    class.prob <- as.data.frame(t(apply(X = order.matrix, MARGIN =  1,
                                        FUN = function(row){LabelCount(row, class.labels)/k})))
    names(class.prob) <- class.labels
    
    
    return(class.prob)
}

#--------------------------------------------------------------------------------
#Calculates order matrix (neighbors) for new data

Predict <- function(features, memory, labels, class.labels, k = 1, p = 2) {
    
    #calculate distance matrix
    dist.matrix <- DistMatrix(features = features, memory = memory, p = p, mode = "test")
    
    #order.matrixrix
    order.matrix <- OrderMatrix(dist.matrix, mode = "test", k = k, labels = labels)
    
    #calculate class probabilities 
    class.prob <- as.data.frame(t(apply(X = order.matrix, MARGIN =  1,
                                        FUN = function(row){LabelCount(row, class.labels)/k})))
    names(class.prob) <- class.labels
    
    return(class.prob)
}

#--------------------------------------------------------------------------------
#Main function for classifying Data
KNNClassifier <- function(features, labels, 
                          operation = "train", k = 1, p = 2, memory = NULL) {
    #Input Validation
    not_empty(features)
    not_empty(labels)
    is.count(k)
    is.count(p)
    assert_that(k %% 2 == 1)
    assert_that(p %in% c(1,2,Inf))
    assert_that(operation %in% c("train", "predict"))
    if(operation == "predict") not_empty(memory)
    
    class.labels <- unique(labels)
    #Calculate Order Matrix
    if(operation == "train") {
        class.prob <- Train(features, labels, class.labels, k, p)
    } else {
        class.prob <- Predict(features, memory, labels, class.labels, k, p)
    }
    
    #Predict classes
    predicted.class <- data.frame(t(apply(class.prob, 1, 
                                          function(row) {c(type = class.labels[which.max(row)], prob = max(row))}
                                          )))
    
    error <- accuracy <- NA
    
    if(operation == "train"){
        error <- table(predicted.class$type, labels)
        accuracy <- mean(predicted.class$type == labels )
    
        } else if(operation == "predict"){
        
        error <- NA
        accuracy <- NA
        
        df <- data.frame(class = predicted.class$type, prob = predicted.class$prob)
        write.csv(df, file = "predictions.csv", row.names = TRUE)
        }
    
    
    return(list(pred.labels = predicted.class$type,
                error = error,
                accuracy = accuracy,
                class.prob = predicted.class$prob))
}

#--------------------------------------------------------------------------------------------------
OptimiseKnn <- function(features, labels, kseq, p) {
    
    #calculate distance matrix
    dist.matrix <- DistMatrix(features = features, p = p, mode = "train")
    
    #order.matrix
    order.matrix <- as.matrix(t(apply(dist.matrix, 1, order))[, -1])
    
    GetError <- function(order.matrix, labels, k, p) {
        class.labels <- unique(labels)
        order.matrix <- order.matrix[,1:k, drop=FALSE]
        
        class.matrix <- matrix(0, nrow = nrow(order.matrix), ncol = ncol(order.matrix))
        
        for(row in 1:nrow(order.matrix)) {
            for(col in 1:ncol(order.matrix)) {
                class.matrix[row, col] <- labels[order.matrix[row, col]]
            }
        }
        
        
        #calculate class probabilities
        class.prob <- as.data.frame(t(apply(X = class.matrix, MARGIN =  1,
                                            FUN = function(row){LabelCount(row, class.labels)/k})))
        names(class.prob) <- class.labels
        
        #choose class with max probability
        predicted.class <- (apply(class.prob, 1, function(row) {type = class.labels[which.max(row)]}
        ))
        
        #calculate error
        error <- 1 - mean(predicted.class == labels)
        return(error)
    }
    
    #detect cores for parallel computing
    cores <- detectCores()
    cl <- makeCluster(cores, type = "FORK")
    registerDoParallel(cl)
    
    error.rate <- parSapply(cl =cl,
                            X = kseq, 
                            function(k) {GetError(order.matrix, labels, k, p)},
                            simplify = TRUE)
    
    stopCluster(cl)
    
    return(data.frame(k = kseq, error = error.rate))
    
}


#Demo
#--------------------------------------------------------------------------------------------------
#create dataset
features <- genWaves()
labels <- features$y
features <- features[, -3]
p <- 2


#Optimise based on k

kseq <- seq.int(from = 1, to = 31, by = 2)

error.DF <- OptimiseKnn(features = features, labels = labels, kseq = kseq, p = p)
#data frams of errors with differet k

#plot
pdf("error_plot.pdf", width=6, height=5)
print(ggplot(data = error.DF) +
          geom_point(aes(x = k, y = error)) +
          geom_line(aes(x = k, y = error)) +
          ggtitle("Error Rates with different K") +
          scale_x_discrete(breaks= kseq, labels=kseq))
dev.off()

#choose best k
best.k <- error.DF$k[which.min(error.DF$error)]

#test new data
test <- genWaves(slice = 0.05, saveData = TRUE, savePlot = TRUE, seed = 12345)
test.features <- test[, -3]
test.summary <- KNNClassifier(features = test.features, memory = features, labels = labels, 
                              operation = "predict", k = best.k, p = 2)

test.summary$error
test.summary$accuracy
test.summary$pred.labels

df <- data.frame(x1 = test$x1, x2 = test$x2, class = as.factor(test.summary$pred.labels))

df.melt <- melt(data = df)
ggplot(data = df) +
    geom_point(aes(x = x1, y =x2, color = class)) +
    stat_density2d()