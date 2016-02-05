library("assertthat")
library("mvtnorm")
library("dplyr")

#--------------------------------------------------------------------------------
#calculate distance between a row and given dataset
calc_dist <- function(row, data, p = 2) {
    #check is length of row is equal to # of columns of data 
    #assert_that(are_equal(length(as.numeric(row)), ncol(data)))
    
    row_matrix <- matrix( rep(as.numeric(row), each = nrow(data)) , nrow = nrow(data))
    
    return((rowSums( abs((data - row_matrix))^p )) ^ (1.0/p))
}

#--------------------------------------------------------------------------------
#Calculates order matrix (neighbors) for training data
train <- function(trainData, k = 1, p = 2) {
    
    #create dist matrix
    distMat <- apply(trainData[,1:2], 1, function(row) calc_dist(row, trainData[,1:2], p))
    
    #OrderMatrix
    orderMat <- as.matrix(t(apply(distMat, 1, order))[,2:(k+1)])
    
    return(orderMat)
}

#--------------------------------------------------------------------------------
#Calculates order matrix (neighbors) for new data

predict <- function(trainData, memory, k = 1, p = 2) {
    
    #calculate distance matrix
    distMat <- apply( trainData[,1:2], 1, function(row) calc_dist(row, memory[,1:2], p))
    
    #OrderMatrix
    orderMat <- as.matrix(t(apply(distMat, 2, order))[,1:k])
    
    return(orderMat)
}

#--------------------------------------------------------------------------------
#Main function for classifying Data
kNN_classifier <- function(trainData, memory = NULL, trueClasses, 
                           operation = "train", k = 1, p = 2) {
    #Input Validation
    not_empty(trainData)
    not_empty(trueClasses)
    is.count(k)
    is.count(p)
    assert_that(k %% 2 == 1)
    assert_that(p %in% c(1,2,Inf))
    assert_that(operation %in% c("train", "predict"))
    if(operation == "predict") not_empty(memory)
    
    #Calculate Order Matrix
    if(operation == "train") {
        orderMat <- train(trainData, k, p)
    }
    else {
        orderMat <- predict(trainData, memory, k, p)
    }
    
    #Predict classes
    predictedClass <- as.character(apply(orderMat, 1, 
                                         function(x) names(which.max(table(trueClasses[x])))))
    
    #Calculate error
    error <- accuracy <- NA
    
    if(operation == "train"){
        error <- table(predictedClass, trueClasses)
        accuracy <- mean(predictedClass == trueClasses )
    }
    else {
        error <- table(predictedClass, trainData[,3])
        accuracy <- mean(predictedClass == trainData[,3])
    }
    
    return(list(predictedClass = predictedClass,
                error = error,
                accuracy = accuracy))
}