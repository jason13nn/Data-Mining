#---- REQUIRED PACKAGES ----#
library(caret)
library(dplyr)
library(Information)
library(glmnet)
library(e1071)
library(randomForest)
library(nnet)

#---- READ THE CSV FILE ----#

raw_data = read.csv("/Users/jason13nn/Desktop/SMU/Fall 2020/CS 7331/Project/smu-data-mining/train.csv", stringsAsFactors = FALSE)
str(raw_data)
#---- DISPLAY THE NO. OF ROWS AND COLUMNS IN THE INPUT DATASET ----#
dim(raw_data) 
test <- read.csv("/Users/jason13nn/Desktop/SMU/Fall 2020/CS 7331/Project/smu-data-mining/test.csv")
dim(test) #150 38
validation <- read.csv("/Users/jason13nn/Desktop/SMU/Fall 2020/CS 7331/Project/smu-data-mining/validation.csv")
dim(validation) #100 38

#############Data Pre-processing#################
#################################################
# CHECK FOR NA VALUES FOR THE COLUMNS
sapply(raw_data, function(x) sum(is.na(x)))

#Check for duplicated rows 

duptf=duplicated(raw_data) #return T/F of each row 

which(duptf) #print index which is T 

#detect error data   

#par(mfrow = c(2, 2))  # Set up a 2 x 2 plotting space 

#to many plots, hard to put all of them in one or two pictures 


# Create the loop.vector (all the columns) 

loop.vector <- 1:37 

for (i in loop.vector) { # Loop over loop.vector 
  
  # store data in column.i as x 
  
  x <- raw_data[,i] 
  
  
  
  # Plot boxplot of x 
  
  boxplot(x, 
          
          main = paste("f", i)) 
  
  out=boxplot.stats(x)$out 
  
  mtext(paste("Outliers: ", paste(out, collapse = ", "))) 
  
} 


#set a cut off value so we can remove some "error" using which function 

#set a cut off value so we can remove some "error" using which function 

invalidIdx=which(raw_data$f10>2000) 

raw_data=raw_data[-invalidIdx, ] 

invalidIdx=which(raw_data$f11>1500) 

raw_data=raw_data[-invalidIdx, ] 

invalidIdx=which(raw_data$f12>100) 

raw_data=raw_data[-invalidIdx, ] 

invalidIdx=which(raw_data$f14>2000) 

raw_data=raw_data[-invalidIdx, ] 

#invalidIdx=which(raw_data$f15>10) 

#raw_data=raw_data[-invalidIdx, ] 

invalidIdx=which(raw_data$f19>100) 

raw_data=raw_data[-invalidIdx, ] 

#invalidIdx=which(raw_data$f22>2) 

#raw_data=raw_data[-invalidIdx, ] 

#invalidIdx=which(raw_data$f23>150) 

#raw_data=raw_data[-invalidIdx, ] 

invalidIdx=which(raw_data$f29>0.2) 

raw_data=raw_data[-invalidIdx, ] 

#invalidIdx=which(raw_data$f32>40) 

#raw_data=raw_data[-invalidIdx, ] 

#invalidIdx=which(raw_data$f35>100) 

#raw_data=raw_data[-invalidIdx, ] 

#invalidIdx=which(raw_data$f36>15) 

#raw_data=raw_data[-invalidIdx, ] 

#invalidIdx=which(raw_data$f37>250) 

#raw_data=raw_data[-invalidIdx, ] 

dim(raw_data) #243 obs remaining

train <- raw_data

##############Data Analysis################
###########################################

###Penalized regressions

train.x <- model.matrix(response~., train)[,-1]
train.y <- train$response

validation.x <- model.matrix(response~., validation)[,-1]
validation.y <- validation$response

##1. Ridge regression
#Cross validation(10-fold CV)
set.seed(20)
cv.out <- cv.glmnet(train.x, train.y, alpha = 0)
#plot(cv.out)

#Identify optimal tuning parameter
bestlam <- cv.out$lambda.min
bestlam #0.139237

train.ridge <- glmnet(train.x, train.y, alpha = 0, lambda = bestlam)

#Make predictions and evaluate the test MSE 
ridge.pred <- predict(train.ridge, s=bestlam, newx=validation.x)
mean((ridge.pred-validation.y)^2) #0.1639627

##2. The lasso
#Cross validation(10-fold CV)
set.seed(20)
cv.out2 <- cv.glmnet(train.x, train.y, alpha = 1)
#plot(cv.out)

#Identify optimal lambda
bestlam2 <- cv.out$lambda.min
bestlam2 #0.01329085

train.lasso <- glmnet(train.x, train.y, alpha = 1, lambda = bestlam2)

#Make predictions and evaluate the test MSE 
lasso.pred <- predict(train.lasso, s=bestlam, newx = validation.x)
mean((lasso.pred-validation.y)^2) #0.1657413

##3.Elastic Net
set.seed(20)
tcontrol <- trainControl(method="repeatedcv", number=10, repeats=5)
#Cut down on the length and spread of the lambda values
tuneParam <- expand.grid(alpha = seq(0.1, 1, 0.1), lambda = 10^seq(2, -2, length=25))

train.en <- train(train.x, train.y, trControl=tcontrol, method="glmnet", tuneGrid=tuneParam)
train.en$bestTune #alpha=0.1, lambda=0.04641589
train.en.final <- train.en$finalModel

#Make predictions and evaluate the test MSE 
en.pred <- predict(train.en.final, 
                   alpha=train.en$bestTune$alpha, 
                   s=train.en$bestTune$lambda,
                   newx = validation.x)

mean((en.pred-validation.y)^2) #0.1642524

##4. Linear Regression
train.lm <- lm(response~., train)
lm.pred <- predict(train.lm, newdata = validation)

mean((lm.pred-validation.y)^2) #0.2166775

#5. SVM

train.svm <- svm(response ~ .,
                 train,scale = TRUE , kernel="polynomial",
                 degree=3, gamma =0.001)

svm.pred <- predict(train.svm, newdata = as.matrix(validation))

mean((svm.pred-validation$response)^2) #0.1904955

#6. Random Forest

train.rf <- randomForest(response~., train)
rf.pred <- predict(train.rf, newdata = validation)

mean((rf.pred-validation.y)^2) #0.1840506

#7. Neural Network

my.grid <- expand.grid(.decay = c(0), .size = c(0))

train.nn <- train(response ~., data = train,
               method = "nnet", maxit = 10000, tuneGrid = my.grid, trace = F, linout = T, skip = TRUE)  

nn.pred <- predict(train.nn, newdata = validation)

mean((nn.pred-validation.y)^2) #0.2712622

################Prediction#####################
###############################################

test.x <- model.matrix(~.,test)[,-c(1,2)]

#Ridge

ridge.pred.test <- predict(train.ridge, s=bestlam, newx=test.x)

submit.ridge <- data.frame(ID = test$ID, Expected = ridge.pred.test)

submit.ridge <- submit.ridge %>% dplyr::rename(Expected = X1)

#output data
write.csv(submit.ridge, file = "/Users/jason13nn/Desktop/SMU/Fall 2020/CS 7331/Project/submission/submit.ridge.csv")

#LASSO

test.x <- model.matrix(~.,test)[,-c(1,2)]

lasso.pred.test <- predict(train.lasso, s=bestlam2, newx=test.x)

submit.lasso <- data.frame(ID = test$ID, Expected = lasso.pred.test)

submit.lasso <- submit.lasso %>% dplyr::rename(Expected = X1)

#output data
write.csv(submit.lasso, file = "/Users/jason13nn/Desktop/SMU/Fall 2020/CS 7331/Project/submission/submit.lasso.csv")

#Elastic Net

en.pred.test <- predict(train.en.final, 
                        alpha=train.en$bestTune$alpha,
                        s=train.en$bestTune$lambda, 
                        newx=test.x)

submit.en <- data.frame(ID = test$ID, Expected = en.pred.test)

submit.en <- submit.en %>% dplyr::rename(Expected = X1)

#output data
write.csv(submit.en, file = "/Users/jason13nn/Desktop/SMU/Fall 2020/CS 7331/Project/submission/submit.en.csv")


