library(stats)
library(graphics)
library(cluster)
library(fpc)
library(caret)
library(e1071)
# The datasets package needs to be loaded to access our data 
# For a full list of these datasets, type library(help = "datasets")
library(datasets)
data(iris)
summary(iris)
plot(iris)
title (sub = 'Distribution initiale of 5 variable')

x <- iris[,1:4]
plot(x)
title(sub = 'Distribution initiale of 4 variable')

# k moyennes clustering
cl3 <- kmeans(x,3)
cl3
plot(x, col=cl3$cluster)
title(sub = "Apres le k-means")

y = cl3
cl3$cluster = gsub("1","setosa",cl3$cluster)
cl3$cluster = gsub("2","versicolor",cl3$cluster)
cl3$cluster = gsub("3","virginica",cl3$cluster)
confusionMatrix(cl3$cluster,iris[,5])

# k medoid clustering
pamx <- pam(x,3)
pamx 
summary(pamx)
plot(pamx)

#knn clustering
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]

knnFit <- train(TrainData, TrainClasses,
                method = "knn",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"))
confusionMatrix(knnFit)
confusionMatrix(knnFit, "average")
confusionMatrix(knnFit, "none")