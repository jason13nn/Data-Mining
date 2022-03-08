# 10/15/2020
# Miju Ahn
# MNIST dataset (download mnist_data.RData from Canvas - Home - Under "Data")

# plot 1 sample 
w <- 28
h <- 28 
firstSample <- Xtrain[3, ]
firstSample

#install.packages("ggplot2")
library(ggplot2)

# create coordinate (x,y) for each pixel
myImg <- data.frame(xVal = rep(1:w, h), # building all possible coordinates using 1:28, 1:28
           yVal = rep(h:1, each = w),
           rgb = firstSample/255) # [0,1] color scale

# plot 
g <- ggplot(data = myImg, aes(x = xVal, y = yVal))
g + geom_point(colour = rgb(myImg[c("rgb", "rgb", "rgb")]))

# Apply KNN
library(FNN)
?knn
knn_pred <- knn(Xtrain, Xtest, Ytrain, k = 5)
# confusion matrix
table(knn_pred, Ytest)
# misclassification rate
mean(knn_pred != Ytest)

# put feature matrix and label together
train <- data.frame(Xtrain, Ytrain)
test <- data.frame(Xtest, Ytest)

# Apply random forest
#install.packages("randomForest")
library(randomForest)

?randomForest
rf_model <- randomForest(Ytrain ~ ., 
                         data = train,
                         mtry = 784)
rf_pred <- predict(rf_model, test)
table(rf_pred, test$Ytest)
mean(rf_pred != test$Ytest)
























