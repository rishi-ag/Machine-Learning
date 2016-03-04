source("cTree.R")

#read data
spam <- read.table("spambase.data", header = F, sep = ",")
features <- spam[, 1: ncol(spam) - 1]
labels <- spam[, ncol(spam)]


#get errors based on my function in vector
error <- c()

system.time(for(depth in seq(2, 7,1)) {
  decision.tree <- cTree(features, labels, depth = depth, minPoints = 5)
  predictions <- PredictData(features = features, decision.tree = decision.tree)
  error <- c(error , 1 - sum(predictions$prediction == labels)/length(labels))  
})


#get errors based on my rpart in vector
rpart.error <- c()
system.time( for(depth in seq(2, 7,1)) {
  control <- rpart.control(maxdepth = depth, minsplit = 5)
  fit <- rpart(V58 ~ ., spam, control=control, method = "class")
  rpart.error <- 1 - c(rpart.error, sum(labels == (as.numeric(predict(fit, type = "class")) - 1))/length(labels))
})


#combine into dataframe for plotting
combined.error <- data.frame(depth = seq(2, 7), my.func.err = error, rpart.error = rpart.error)
error.melt <- melt(data = combined.error, id.vars = 1)

#save
pdf("error_evolution.pdf")
(ggplot(error.melt)+
  geom_line(aes(x = depth, y = value, col = variable)) +
  geom_point(aes(x = depth, y = value, col = variable))
)
dev.off()

dev.copy(png,"error_evolution.png")
dev.off()
