library(ggplot2)
library(mvtnorm)
library(assertthat)


GenData <- function(no.classes, param.vector, save.csv = TRUE, save.pdf = TRUE){
    #-------------------------------------------------------------
    #Function returns dataset characterised by parameters passed (see GetSpiral).
    #Also saves csv file of data and a plot of data if repective flags set to TRUE
    
    #check that no.classes and length of parameter list is same
    assert_that(no.classes == length(param.vector))
    
    #generate data
    data <- data.frame(x = as.numeric(), y = numeric(), type = numeric())
    
    #generate data for each class based on the parameters specified 
    for(i in 1:no.classes) {
        temp <- GetSpiral(param.vector[[i]]$n, param.vector[[i]]$a, param.vector[[i]]$b,
                          param.vector[[i]]$theta, param.vector[[i]]$step, param.vector[[i]]$disturbance,
                          param.vector[[i]]$cov.factor, param.vector[[i]]$class.name)
        data <- rbind(data, temp)
        
    }
    
    #import csv if save.csv flag raised
    if(save.csv == TRUE){
        write.csv(data, file = "data.csv", row.names = FALSE)
    }
    
    #import pdf of plot if save.pdf flag raised
    if(save.pdf == TRUE)  {
        data$type <- as.factor(data$type)
        pdf("plot.pdf", width=6, height=5)
        print(PlotGraph(data = data, title = "Cyclones types based spiral distance", 
                        size = 0.8, x.label = "Demeaned diameter", y.label = "Demeaned diameter"))
        dev.off()
    }
    
    return(data)
}

GetSpiral <- function(n, a, b, theta, step, disturbance , cov.factor, class.name) {
    
    #function returns data that follows a spiral using the polar coordinates equation polar = a + b * theta (see below)
    #1.   if n is not divisible disturbances then points returned will be more than n
    #2.   generally higher the value of parameter b, more the number of perfectly non linearly seperatable 
    #     classes that can be specified
    #3.   generally higher the value of parameter a, more the distance seperating different classes 
    #------------------------------------------------------------------------------
    #parameter space
    #------------------------------------------------------------------------------
    #a     = numerical constant in R; controls turn of the spiral
    #b     = numerical constant in R controls the distance between arms of the spiral
    #theta = instial angle in radians of the spiral 
    #step  = successive increase in theta
    #disturbance = No of points randomly generated using cartesian coordiantes of a given point as mean
    #cov.factor = factor by which multiplies by the variance covariance matrix of each point
    #class.name = Name of class
    #------------------------------------------------------------------------------
    
    n <- round(n/disturbance) #change n by a factor of disurbances
    data <- data.frame(x = as.numeric(), y = numeric())
    
    for(i in 0 :(n -1)) {
        polar <- a + b * (theta + i * step)
        coords <- c(polar * cos(theta + i * step), polar * sin(theta + i * step))
        data.temp <- as.data.frame(rmvnorm(n = disturbance, mean = coords, sigma = cov.factor * diag(2)))
        data <- rbind(data, data.temp)
    }
    
    names(data) <- c("x", "y")
    data$type <- rep(class.name, n)
    data
}


PlotGraph <- function(data, title = "", x.label = "", y.label = "", size = 0.8){
    p <- ggplot(data = data, aes(x = x, y= y, col = type)) +
        geom_point(size = size) +
        scale_x_continuous(name = x.label) +
        scale_y_continuous(name = y.label) +
        ggtitle(title)
}

#-------------------------------------------------------------
#-------------------------------------------------------------


param.vector <- list(class1 = list(n = 1000, a = 10, b = 20, theta = pi/6, step = 0.03, disturbance = 3,
                                cov.factor = 10, class.name = "A"),
                     class2 = list(n = 1000, a = 50, b = 20, theta = pi/6, step = 0.03, disturbance = 3,
                                cov.factor = 6, class.name = "B"),
                     class3 = list(n = 1000, a = 100, b = 20, theta = pi/6, step = 0.03, disturbance = 3,
                                cov.factor = 6, class.name = "C"))

data <- GenData(no.classes = 3, param.vector = param.vector)