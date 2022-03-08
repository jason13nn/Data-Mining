# 10/20/2020
# Miju Ahn
# SVM

# breast_cancer.csv
myData <- read.csv("/Users/jason13nn/Desktop/SMU/Fall 2020/CS 7331/data/breast_cancer.csv", header = TRUE)
str(myData)

# change "diagnosis" to type factor
myData$diagnosis <- as.factor(myData$diagnosis)

# split into train/test
set.seed(100)
trainIdx <- sample(1:569, 300)
train <- myData[trainIdx, ]
test <- myData[-trainIdx, ]

# package for SVM
#install.packages("e1071")
library(e1071)
?svm

# SVM model 1
m <- svm(diagnosis ~ ., data = train)
summary(m)
pred <- predict(m, test)
pred[1:10]
test$diagnosis[1:10]

# confusion matrix
table(pred, test$diagnosis)
(14+3)/269

# misclassification rate
mean(pred != test$diagnosis) # 6.3 %

# SVM model 2
m2 <- svm(diagnosis ~., 
          data = train,
          kernel = "polynomial",
          degree = 3,
          gamma = 2,
          coef0 = 1
          )
pred2 <- predict(m2, test)

# model performance
table(pred2, test$diagnosis)
mean(pred2 != test$diagnosis) # 7.8 %

