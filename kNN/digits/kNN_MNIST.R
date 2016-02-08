if(!require("ggplot2")) install.packages("ggplot2")
if(!require("dplyr")) install.packages("dplyr")
if(!require("doParallel")) install.packages("doParallel")
if(!require("class")) install.packages("class")
source("./kNN.R")

#' I hve written a paralleised optimization function for k in kNN.R but this dataset is too big
#' for that function to handle. SO I will resort to using the knn function in package class


##Sourced Files
rm()
#Training Data
train.data <- read.csv("./digits/MNIST_training.csv", header = F)
true.classes <- train.data[,1]
train.data <- train.data[,-1]

#Test Data
test.data <- read.csv("./digits/MNIST_test.csv", header = F)


predictions <- knn(train = train.data, test = test.data, k = 1, cl = true.classes)
df <- data.frame(class = predictions)
write.csv(df, file = "MNIST_predictions.csv", row.names = TRUE)
