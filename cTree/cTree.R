library(plyr)
library(ggplot2)
library(mvtnorm)
source("genData.R")
#classification trees

#==================================================================
Entropy <- function(labels){
  N <- length(labels)
  label.counts <- count(labels)$freq
  entropy.vec <- sapply(label.counts, function(count) -(count/N) * log2(count/N))
  return(sum(entropy.vec))
}

##test case
#labels <- c(rep("a", 10), rep("b", 10), rep("c", 10))
#labels1 <- c(rep("a", 6), rep("b", 6))

#Entropy(labels) #expected value =  1.584963
#Entropy(labels1)
#==================================================================

#==================================================================
ExpectedEntropy <- function(parent.labels, children.labels) {
  N <- length(parent.labels)
  expected.entropy <- sum(sapply(children.labels, function(child) (length(child)/ N)* Entropy(child)))
  return(expected.entropy)
}

##test case
#parent.labels <- c(rep("a", 6), rep("b", 6))
#children.labels <- list(rep("a",2), rep("b",4), c(rep("b",2), rep("a",4)))
#ExpectedEntropy(parent.labels, children.labels) # expected answer = expected.entropy


#==================================================================
BestThresholdEntropy <- function(feature, labels, no.thresh = 10, diagnostics = FALSE) {
  
  
  # create potential thresholds vector
  pot.thresh <- seq(min(feature), max(feature), length.out = (no.thresh + 2))
  pot.thresh <- pot.thresh[2: (no.thresh + 1)]
  #evaluate inforation gain at each threshold 
  
  information.gain.vec <- sapply(pot.thresh, 
                                 function(thresh) Entropy(labels) - ExpectedEntropy(labels, 
                                                                      list(labels[feature > thresh], 
                                                                           labels[!(feature > thresh)])))
  
  best.thresh <- pot.thresh[which.max(information.gain.vec)]
  
  if(diagnostics == FALSE){
    return(c(best.thresh = best.thresh, information.gain = max(information.gain.vec)))
  }
  else {
    
    return(data.frame(threshold = pot.thresh, information.gain = information.gain.vec))
  }
}

#test
#labels1 <- c(rep("a", 15), rep("b", 5), rep("c", 10))
#BestThresholdEntropy(rnorm(100), sample(c("a", "b"), 100, replace = T), 10)
#feature <- features[,1]; labels <- labels
#BestThresholdEntropy(features[,1], labels, 10, diag = T)
#==================================================================

ChooseSplit <- function(features, labels, method, diagnostics = FALSE){
  
  if ( (tolower(method) == "entropy") | (method = 1) ) {
    information.gain <-  as.data.frame(t(apply(features, 2, function(feature) BestThresholdEntropy(feature, labels))))
  }
  
  information.gain$feature <- rownames(information.gain)
  
  if(diagnostics == FALSE){
    best.cut <- information.gain[which.max(information.gain$information.gain),]
    return(best.cut)
  }
  else {
    
    return(information.gain)
  }
  
}
#==================================================================

SplitData <- function(features, labels, rule) {
  child1 <- features[[rule[1, 4]]] < rule[1, 2] 
  child2 <- !child1
  
  features1 <- features[child1,]
  labels1 <- labels[child1]
  prob1 <- count(labels1)
  #prob1$freq <- prob1$freq/length(labels1)  
  
  features2 <- features[child2,]
  labels2 <- labels[child2]
  prob2 <- count(labels2)
  #prob2$freq <- prob2$freq/length(labels2)  
  
  return(list(prob1 = prob1$freq[which.max(prob1$freq)]/length(labels1),
              class1 = as.character(prob1$x[which.max(prob1$freq)]),
              feat1 = features1, 
              lab1 = labels1,
              no.points1 = length(labels1),
              prob2 = prob2$freq[which.max(prob2$freq)]/length(labels2),
              class2 = as.character(prob2$x[which.max(prob2$freq)]),
              feat2 = features2, 
              lab2 = labels2,
              no.points2 = length(labels2)))
}
#==================================================================
BuildCTree <- function(features, labels, depth = 3, min.points = 10){
  depth.queue <- list()
  depth.queue[[length(depth.queue) + 1]] <- list(depth = 1, features = features, labels = labels)
  
  decision.rules <- data.frame(split = numeric(),
                               best.thresh = double(),
                               information.gain = double(),
                               feature = character())

  depth.prob <- data.frame(depth = character(),
                           prob1 = double(),
                           class1 = factor(),
                           no.points1 = numeric(),
                           prob2 = double(),
                           class2 = factor(),
                           no.points2 = numeric())
  #k = 1
  for (k in 1:depth){
    temp.queue <- list()
    counter <- 0
    while (length(depth.queue) != 0) {
      counter <-  counter + 1
      level <- paste0(as.character(k), letters[counter])
      split <- depth.queue[[1]]
      depth.queue[[1]] <- NULL
      
      decision.rules <- rbind(decision.rules, cbind(level, ChooseSplit(split$features, split$labels, 1, FALSE)))
      
      new.split <- SplitData(split$features, split$labels, decision.rules[nrow(decision.rules),]) 
      
      depth.prob <- rbind(depth.prob, cbind(level,
                                           round(new.split$prob1, 3),
                                           new.split$class1,
                                           new.split$no.points1,
                                           round(new.split$prob2, 3),
                                           new.split$class2,
                                           new.split$no.points2))
      
      if(new.split$prob1 != 1 | length(new.split$lab1) > min.points) {
        temp.queue[[length(temp.queue) + 1]] <- list(depth = k + 1, features = new.split$feat1, labels = new.split$lab1)
      }
      
      if(new.split$prob2 != 1 | length(new.split$lab2) > min.points) {
        temp.queue[[length(temp.queue) + 1]] <- list(depth = k + 1, features = new.split$feat2, labels = new.split$lab2)
      }
    }
    
    depth.queue <- c(depth.queue, temp.queue)
  }
  
  names(depth.prob) <- c("level", "prob.left", "class.left", "no.points.left","prob.right", "class.right", "no.points.right")
  
  output <- merge(depth.prob, decision.rules, by.x = "level", by.y = "level")
  return(output)
}

#==================================================================

set.seed(1000)
param.vector <- list(
  class1 = list(no = 50, rho = -0.7, sd = c(10, 20), mean = c(20, 60), class.name = "Approved"),
  class2 = list(no = 50, rho = -0.1, sd = c(20, 20), mean = c(40, 80), class.name = "Denied"),
  class3 = list(no = 50, rho = 0.5, sd = c(15, 12), mean = c(20, 25), class.name = "Undecided")
)

data <- GenData(param.vector)[,1:3]
features <- data[,1:2]
labels <- data[,3]


split <- ChooseSplit(features, labels, 1, FALSE)



output <- BuildCTree(features, labels, depth = 4, min.points = 10)
