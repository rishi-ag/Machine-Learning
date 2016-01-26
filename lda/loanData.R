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
        sim.data <- GenBVN(param.vector[[class]]$no, seed = 5341, param.vector[[class]]$mean, variance)
        sim.data$class <- rep(param.vector[[class]]$class.name, param.vector[[class]]$n)
        data <- rbind(data, sim.data)
    }
    dummy <- as.data.frame(model.matrix( ~ class -1 , data = data))
    names(data)[1:2] <- c("x", "y")
    data <- cbind(data, dummy)
  return(data)
}

#Plot simulated data
PlotGraph <- function(data, title = "", x.label = "", y.label = ""){
    p <- ggplot(data = data, aes(x = x, y= y)) +
        geom_point() +
        scale_x_continuous(name = x.label) +
        scale_y_continuous(name = y.label) +
        ggtitle(title)
    p
}


param.vector <- list(
    class1 = list(no = 1000, rho = -0.2, sd = c(2, 8), mean = c(6, 30), class.name = "Approved"),
    class2 = list(no = 1000, rho = 0.8, sd = c(3.5, 12), mean = c(12, 50), class.name = "Denied"),
    class3 = list(no = 1000, rho = 0.02, sd = c(1, 3), mean = c(4, 10), class.name = "Undecided")
)

data <- GenData(param.vector)
data$intercept <- rep(1, nrow(data))

X <- as.matrix(data[,c("intercept", "x", "y")])
Y <- as.matrix(data[,c("classApproved", "classDenied", "classUndecided")])

weights <- solve(t(X)%*%X) %*% t(X) %*% Y


pred <- X %*% weights
pred.labels <- factor(apply(pred, 1, function(x) which.max(x)), labels = c("Approved", "Denied", "Undecided"))

table(data$class, pred.labels)


PlotGraph(data, "Linear Classification", "solvency", "PI ratio")
#Perform K regressions and classify labels and return coefficiemts and labels

plotBoundary(optim.weights, data)
#Plot the decision boundaries

weights =optim.weights
plotBoundary <- function(weights, data){
  #Calculating points corresponding to boundary lines
  x <- seq(min(data$x), max(data$x), 0.01)
  
  w.c1 <- weights[,1]
  w.c2 <- weights[,2]
  w.c3 <- weights[,3]
  
  y1 <- -((w.c1[3] - w.c2[3]) / (w.c1[2] - w.c2[2])) * x + 
    ((w.c1[1] - w.c2[1]) / (w.c1[2] - w.c2[2]))
  
  y2 <- -((w.c2[3] - w.c3[3]) / (w.c2[2] - w.c3[2])) * x + 
      ((w.c2[1] - w.c3[1]) / (w.c2[2] - w.c3[2]))
  
  y3 <- -((w.c1[3] - w.c3[3]) / (w.c1[2] - w.c3[2])) * x + 
      ((w.c1[1] - w.c3[1]) / (w.c1[2] - w.c3[2]))
  
  
  boundDF1 <- data.frame(x = x , y = y1)
  boundDF2 <- data.frame(x = x , y = y2)
  boundDF3 <- data.frame(x = x , y = y3)
  boundDF1 <- boundDF1[ boundDF1$y < max(data$y) +10 & boundDF1$y > min(data$y) -10, ]
  boundDF2 <- boundDF2[ boundDF2$y < max(data$y) +10 & boundDF2$y > min(data$y) -10, ]
  boundDF3 <- boundDF3[ boundDF3$y < max(data$y) +10 & boundDF3$y > min(data$y) -10, ]
  
  PlotGraph(data, "Linear Classification", "solvency", "PI ratio") + 
    geom_line(data = boundDF1) 
  
  +
    geom_line(data = boundDF2) +
    geom_line(data = boundDF3)
}