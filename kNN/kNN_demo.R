source("./genData.R")
source("./kNN.R")

#Demo
#--------------------------------------------------------------------------------------------------
#create dataset
features <- genWaves()
labels <- features$y
features <- features[, -3]
p <- 2


#Optimise based on k

kseq <- seq.int(from = 1, to = 31, by = 2)

error.DF <- OptimiseKnn(features = features, labels = labels, kseq = kseq, p = p)
#data frams of errors with differet k

#plot
pdf("error_plot.pdf", width=6, height=5)
print(ggplot(data = error.DF) +
          geom_point(aes(x = k, y = error)) +
          geom_line(aes(x = k, y = error)) +
          ggtitle("Error Rates with different K") +
          scale_x_discrete(breaks= kseq, labels=kseq))
dev.off()

#choose best k
best.k <- error.DF$k[which.min(error.DF$error)]

#test new data
test <- genWaves(slice = 0.05, saveData = TRUE, savePlot = TRUE, seed = 12345)
test.features <- test[, -3]
test.summary <- KNNClassifier(features = features, memory = features, labels = labels, 
                              operation = "predict", k = best.k, p = 2)

test.summary$error
test.summary$accuracy
test.summary$pred.labels

#------------------------------------------------------------------------------
#plotting

unlabeled.data <- data.frame(
    grid.x = features[,1],
    grid.y = features[,2],
    grid.z = labels 
)

labeled.data <- data.frame(
    feat1 = features[,1],
    feat2 = features[,2],
    class = labels,
    correct = (labels == test.summary$pred.labels)
)

# plot boundaries with contour. looks quite ugly because of the gap in the rough grid
plot1 <- (ggplot(labeled.data, aes(feat1, feat2))
          + geom_point(aes(color = correct))
          + stat_contour(aes(grid.x, grid.y, z = as.numeric(grid.z == 1)), unlabeled.data, bins = 1, alpha = 0.5)
          + stat_contour(aes(grid.x, grid.y, z = as.numeric(grid.z == 2)), unlabeled.data, bins = 1, alpha = 0.5)
          + stat_contour(aes(grid.x, grid.y, z = as.numeric(grid.z == 3)), unlabeled.data, bins = 1, alpha = 0.5)
          + theme(legend.position = "none")
)

plot2 <- (ggplot(labeled.data, aes(feat1, feat2))
          + geom_point(aes(color = correct))
          + geom_tile(aes(grid.x, grid.y, fill = grid.z), unlabeled.data, alpha = 0.2)
          + theme(legend.position = "none")
)
grid.arrange(plot1, plot2, ncol=2)
