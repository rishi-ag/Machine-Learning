library(ggplot2)
library(mvtnorm)
library(assertthat)

n = 1000
a = 10
b = 20
theta = pi/6
step = 0.03
disturbance = 5
cov.factor = 6



GenData <- function(no.classes, param.vector, save.csv = TRUE, save.pdf = TRUE){
    #-------------------------------------------------------------
    
    #check that no.classes and length of parameter list is same
    assert_that(no.classes == length(param.vector))
    
    #generate data
    data <- data.frame(x = as.numeric(), y = numeric(), type = numeric())
    
    for(i in 1:no.classes) {
        temp <- GetSpiral(param.vector[[i]][1], param.vector[[i]][2], param.vector[[i]][3],
                          param.vector[[i]][4], param.vector[[i]][5], param.vector[[i]][6],
                          param.vector[[i]][7])
        temp$type <- rep(i, param.vector[[i]][1])
            
        data <- rbind(data, temp)
        
    }
    data$type <- as.factor(data$type)
    
    
    
    #import csv is flag raised
    if(save.csv == TRUE){
        write.csv(data, file = "data.csv", row.names = FALSE)
    }
    
    if(save.pdf == TRUE)  {
        pdf("plot.pdf", width=5, height=5)
        print(ggplot(data = data, aes(x = x, y= y, col = type)) + geom_point())
        dev.off()
    }
    
    return(data)
}

GetSpiral <- function(n, a, b, theta, step, disturbance , cov.factor) {
    n <- n/disturbance
    data <- data.frame(x = as.numeric(), y = numeric())
    
    for(i in 0 :(n -1)) {
        polar <- a + b * (theta + i * step)
        coords <- c(polar * cos(theta + i * step), polar * sin(theta + i * step))
        data.temp <- as.data.frame(rmvnorm(n = disturbance, mean = coords, sigma = cov.factor * diag(2)))
        data <- rbind(data, data.temp)
    }
    
    names(data) <- c("x", "y")
    data
}

param.vector <- list(class1 = c(n = 2000, a = 10, b = 30, theta = pi/6, step = 0.03, disturbance = 5,
                                cov.factor = 10),
                     class2 = c(n = 2000, a = 30, b = 30, theta = pi/6, step = 0.03, disturbance = 5,
                                cov.factor = 6),
                     class3 = c(n = 2000, a = 50, b = 30, theta = pi/6, step = 0.03, disturbance = 5,
                                cov.factor = 6))

data <- GenData(no.classes = 3, param.vector = param.vector)


ggplot(data = data, aes(x = x, y= y, col = as.factor(type))) + geom_point()
