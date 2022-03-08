#read data
library(MASS)
myData <- Cars93

##Pre-process
#1
str(myData)
#2
summary(myData) #Yes. Rear.seat.room:2, Luggage.room:11
myData <- na.omit(myData)
#3
myData <- Filter(is.numeric,myData)
#4
train <- myData[1:40,]
validation <- myData[41:60,]
test <- myData[-c(1:60),]

##Linear Regression
#5
lm.myData <- lm(Horsepower ~ ., train)
summary(lm.myData)
#6
#RPM, EngineSize, and Weight
#7
lm.pred <- predict(lm.myData, test)
lm_mse <- mean((lm.pred - test$Horsepower)^2)
lm_mse

##Ridge Regression
library(glmnet)
#8
train2 <- as.matrix(train)
test2 <- as.matrix(test)
validation2 <- as.matrix(validation)

ridge.validation2 <- cv.glmnet(validation2[,-7], validation2[,7], 
                          alpha = 0, lambda = c(0.01, 0.1, 1, 10))
plot(ridge.validation2)
#The model has the lowerest MSE when lambda = 1.
ridge.validation2$lambda.min
#9
ridge.myData <- glmnet(train2[ ,-7],
             train2[ , 7],
             family = "gaussian",
             alpha = 0,
             lambda = 1)

#Estimated equation
ridge.myData$a0
ridge.myData$beta
#10
ridge_pred <- predict(ridge.myData, test2[ ,-7])
ridge_mse <- mean((ridge_pred - test2[ ,7])^2)
ridge_mse

##LASSO
#11
lasso.myData.val <- cv.glmnet(validation2[,-7], validation2[,7], 
                          alpha = 1, lambda = c(0.01, 0.1, 1, 10), 
                          nfolds = 4)
lasso.myData.val$lambda.min 
#The model has the lowerest MSE when lambda = 0.1.
#12
lasso.myData <- glmnet(train2[ ,-7],
                       train2[ , 7],
                       family = "gaussian",
                       alpha = 1,
                       lambda = 0.1)
#Estimated equation
lasso.myData$a0
lasso.myData$beta
#13
lasso_pred <- predict(lasso.myData, test2[ ,-7])
lasso_mse <- mean((lasso_pred - test2[ ,7])^2)
lasso_mse

#To compare MSE
#Linear Rregression: 266.8284
#Ridge Regression: 256.0473
#LASSO: 230.1024