#Wrapper functions

#Create variance covariance matrix
SigmaXY <- function(rho, sdX, sdY) {
    covTerm <- rho * sdX * sdY
    VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 
                       2, 2, byrow = TRUE)
    return(VCmatrix)
}

#Create a Bivariate Normal Distribution
GenBVN <- function(n = 1, seed = NA, muXY=c(0,1), SigmaXY=diag(2)) {
    if(!is.na(seed)) set.seed(seed)
    rdraws <- rmvnorm(n, mean = muXY, sigma = SigmaXY)
    return(as.data.frame(rdraws))
}


#Create a dataset for classification
GenData <- function(param.vector, seed = 5341) {
    
    data <- data.frame(x = as.numeric(), y = as.numeric(), class = as.character())
    
    for(class in 1:length(param.vector)) {
        variance <- SigmaXY(param.vector[[class]]$rho, param.vector[[class]]$sd[1], param.vector[[class]]$sd[2])
        sim.data <- GenBVN(param.vector[[class]]$no, 5341, param.vector[[class]]$mean, variance)
        sim.data$class <- rep(param.vector[[class]]$class.name, param.vector[[class]]$n)
        data <- rbind(data, sim.data)
    }
    dummy <- as.data.frame(model.matrix( ~ class -1 , data = data))
    names(data)[1:2] <- c("x", "y")
    data <- cbind(data, dummy)
    return(data)
}

#classify data using lda
Classify <- function(data){
    
    data$intercept <- rep(1, nrow(data))
    
    X <- as.matrix(data[,c("intercept", "x", "y")])
    Y <- as.matrix(data[,c("classApproved", "classDenied", "classUndecided")])
    
    weights <- solve(t(X)%*%X) %*% t(X) %*% Y
    
    
    pred <- X %*% weights
    pred.labels <- factor(apply(pred, 1, function(x) which.max(x)), labels = c("Approved", "Denied", "Undecided"))
    
    return(list(weights = weights, prediction = pred.labels))
}

#Plot the decision boundaries
PlotBoundary <- function(weights,  data){
    
    w1 <- weights[, 1]
    w2 <- weights[, 2]
    w3 <- weights[, 3]
    
    #Calculating points corresponding to boundary lines
    x1 <- seq(min(data$x), max(data$x), 0.01)
    y1 <- -((w1[3] - w2[3]) / (w1[2] - w2[2])) * x1 + 
        ((w2[1] - w1[1]) / (w1[2] - w2[2]))
    y2 <- -((w3[3] - w1[3]) / (w3[2] - w1[2])) * x1 + 
        ((w1[1] - w3[1]) / (w3[2] - w1[2]))
    y3 <- -((w2[3] - w3[3]) / (w2[2] - w3[2])) * x1 + 
        ((w3[1] - w2[1]) / (w2[2] - w3[2]))
    
    
    boundDF1 <- data.frame(x = x1 , y = y1, Animal=rep("Bound1", length(x1)))
    boundDF2 <- data.frame(x = x1 , y = y2, Animal=rep("Bound2", length(x1)))
    boundDF3 <- data.frame(x = x1 , y = y3, Animal=rep("Bound3", length(x1)))
    boundDF1 <- boundDF1[ boundDF1$y < max(data$y) +10 & boundDF1$y > min(data$y) -10, ]
    boundDF2 <- boundDF2[ boundDF2$y < max(data$y) +10 & boundDF2$y > min(data$y) -10, ]
    boundDF3 <- boundDF3[ boundDF3$y < max(data$y) +10 & boundDF3$y > min(data$y) -10, ]
    
    p <- ggplot(data = data, aes(x = x, y = y)) + 
        geom_point(aes(col = class)) +
        xlab("solvency") +
        ylab("PI ratio") +
        theme_bw(base_size = 14, base_family = "Helvetica") + 
        geom_line(data = boundDF1) +
        geom_line(data = boundDF2) +
        geom_line(data = boundDF3) +
        ggtitle("Decision Boundaries")
    
    p
}

