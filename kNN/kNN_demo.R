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
test.summary <- KNNClassifier(features = test.features, memory = features, labels = labels, 
                              operation = "predict", k = best.k, p = 2)

test.summary$error
test.summary$accuracy
test.summary$pred.labels