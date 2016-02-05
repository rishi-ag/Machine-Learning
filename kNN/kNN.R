library("assertthat")
library("mvtnorm")
source("./genData.R")
#--------------------------------------------------------------------------------
#calculate distance between a row and given dataset
CalcDist <- function(row, data, p = 2) {
    #check is length of row is equal to # of columns of data 
    #assert_that(are_equal(length(as.numeric(row)), ncol(data)))
    
    row.matrix <- matrix( rep(as.numeric(row), each = nrow(data)) , nrow = nrow(data))
    
    return((rowSums( abs((data - row.matrix))^p )) ^ (1.0/p))
}

#--------------------------------------------------------------------------------
LabelCount <- function(vec, class.type){
    freq <- vector()
    for(class in class.type){
        freq <- c(freq, sum(vec == class))
    }
    return(freq)
}
#--------------------------------------------------------------------------------
#Calculates order matrix (neighbors) for Training data
Train <- function(features, labels, class.type, k = 1, p = 2) {
    
    #create dist matrix
    dist.mat <- apply(features[,1:2], 1, function(row) CalcDist(row, features[,1:2], p))
    
    #order.matrix
    order.mat <- as.matrix(t(apply(dist.mat, 1, order))[,2:(k+1)])
    class.mat <- matrix(0, nrow = nrow(order.mat), ncol = ncol(order.mat))
    for(row in 1:nrow(order.mat)) {
        for(col in 1:ncol(order.mat)) {
            class.mat[row, col] <- labels[order.mat[row, col]]
        }
    }
    
    class.prob <- as.data.frame(t(apply(X = class.mat, MARGIN =  1,
                                        FUN = function(row){LabelCount(row, class.type)/k})))
    names(class.prob) <- class.type
    return(class.prob)
}

#--------------------------------------------------------------------------------
#Calculates order matrix (neighbors) for new data

Predict <- function(features, memory, labels, class.type, k = 1, p = 2) {
    
    #calculate distance matrix
    dist.mat <- apply( features[,1:2], 1, function(row) CalcDist(row, memory[,1:2], p))
    
    #order.matrix
    order.mat <- as.matrix(t(apply(dist.mat, 2, order))[,1:k])
    
    class.mat <- matrix(0, nrow = nrow(order.mat), ncol = ncol(order.mat))
    for(row in 1:nrow(order.mat)) {
        for(col in 1:ncol(order.mat)) {
            class.mat[row, col] <- labels[order.mat[row, col]]
        }
    }
    
    class.prob <- as.data.frame(t(apply(X = class.mat, MARGIN =  1,
                                        FUN = function(row){LabelCount(row, class.type)/k})))
    names(class.prob) <- class.type
    return(class.prob)

}

#--------------------------------------------------------------------------------
#Main function for classifying Data
KNNClassifier <- function(features, memory = NULL, labels, 
                           operation = "train", k = 1, p = 2) {
    #Input Validation
    not_empty(features)
    not_empty(labels)
    is.count(k)
    is.count(p)
    assert_that(k %% 2 == 1)
    assert_that(p %in% c(1,2,Inf))
    assert_that(operation %in% c("train", "predict"))
    if(operation == "predict") not_empty(memory)
    
    class.type <- unique(labels)
    #Calculate Order Matrix
    if(operation == "train") {
        class.prob <- Train(features, labels, class.type, k, p)
    } else {
        class.prob <- Predict(features, memory, labels, class.type, k, p)
    }
    
    #Predict classes
    predicted.class <- data.frame(t(apply(class.prob, 1, function(row) c(type = class.type[which.max(row)], prob = max(row)))))
    
    error <- accuracy <- NA
    
    if(operation == "train"){
        error <- table(predicted.class$type, labels)
        accuracy <- mean(predicted.class$type == labels )
    } else {
        
        error <- table(predicted.class$type, features[,3])
        accuracy <- mean(predicted.class$prob == features[,3])
        
        df <- data.frame(class = predicted.class$type, prob = predicted.class$prob)
        write.csv(df, file = "predictions.csv", row.names = TRUE)
    }
    
    return(list(pred.labels = predicted.class$type,
                error = error,
                accuracy = accuracy,
                class.prob = predicted.class$prob))
}

#--------------------------------------------------------------------------------------------------
#Demo

#create dataset
features <- genWaves()
labels <- features$y

#trainig data with best k

k_seq <- seq.int(from = 1, to = 31, by = 2)

train_error <- sapply(k_seq, 
                      function(k) {
                          (1 - KNNClassifier(features =  features, labels = labels, operation = "train", k = k)$accuracy)
                          })

#data frams of errors with differet k
error.DF <- data.frame(k = k_seq, error = train_error)

#plot
pdf("error_plot.pdf", width=6, height=5)
print(ggplot(data = error.DF) +
          geom_point(aes(x = k, y = error)) +
          geom_line(aes(x = k, y = error)) +
          ggtitle("Error Rates with different K") +
          scale_x_discrete(breaks= k_seq, labels=k_seq))
dev.off()

#choose best k
best.k <- error.DF$k[which.min(error.DF$error)]

#train with best k
summary <- KNNClassifier(features = features, memory = NULL, labels = labels, 
                         operation = "train", k = best.k, p = 2)

summary$error
#test new data
test <- genWaves(slice = 0.05, saveData = TRUE, savePlot = TRUE, seed = 12345)
test.summary <- KNNClassifier(features = test, memory = features, labels = labels, 
                         operation = "predict", k = best.k, p = 2)

test.summary$error
