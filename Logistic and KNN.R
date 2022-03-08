# Download "breast_cancer.csv" from Canvas

# load data/observe data
myData <- read.csv("breast_cancer.csv", header = TRUE)
head(myData)
str(myData)
summary(myData)

# how many patients have cancer?
table(myData$diagnosis)

# split train/test
set.seed(0) # will give us same results after using functions with random components
trainIdx <- sample(569, 400)
train <- myData[trainIdx, ]
test <- myData[-trainIdx, ]

# logistic regression
?glm
m1 <- glm(diagnosis ~ ., 
          data = train,
          family = "binomial")
summary(m1)

# make probability 
m1_prob <- predict(m1, test, type = "response") # will return probability
m1_prob[1:5]

# convert probability to classes
m1_pred <- rep(0, nrow(test))
m1_pred[(m1_prob > 0.5)] <- 1

test$diagnosis[1:5]
m1_pred[1:5]

# model performance
# confusion matrix 
table(test$diagnosis, m1_pred)

# misclassification rate ((1+10)/(59+10+1+99))
mean(test$diagnosis != m1_pred) # 0.065

# K-nearest neighbor 
install.packages("FNN")
library(FNN)
?knn

# change response column to factor
train$diagnosis <- as.factor(train$diagnosis)
test$diagnosis <- as.factor(test$diagnosis)

# KNN
m2_pred <- knn(train[ ,-6], test[ ,-6], train[ ,6], k=3) # returns prediction
m2_pred[1:5]
test$diagnosis[1:5]

# misclassification error
mean(m2_pred != test$diagnosis) # 0.118

# try to use CV to choose number of neighbors 

# plot predictions
?plot
plot(test$mean_radius, test$mean_texture,
     col = m2_pred,
     pch = 19, 
     cex = 0.5,
     xlab = "radius",
     ylab = "texture",
     main = "KNN prediction (k=3)")





