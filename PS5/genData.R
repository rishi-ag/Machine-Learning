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
