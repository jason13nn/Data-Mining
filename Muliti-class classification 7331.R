#read data
myData <- read.csv("/Users/jason13nn/Desktop/SMU/Fall 2020/CS 7331/data/abalone.csv", header = TRUE, sep = ",")

###Pre-process
#0. Change data type of ‘Gender’ to factor
myData$Gender <- as.factor(myData$Gender)

#1. Check ratio between Infant, Female and Male
table(myData$Gender) #F:1307 I:1342 M:1528

#2.Split the dataset into training and testing sets
set.seed(0)
splitPerc <- 0.6
trainIndices = sample(1:dim(myData)[1],round(splitPerc * dim(myData)[1]))
train <- myData[trainIndices,]
test <- myData[-trainIndices,]

###Compare Naive Bayes, LDA, QDA and AdaBoost

library(e1071) #NB
library(MASS) #LDA/QDA
library(adabag) #AdaBoost

#3.Which method yields the lowest testing error?

##Naive Bayes
nb <- naiveBayes(Gender~., train)
nb.pred <- predict(nb, test)

# misclassification rate
mean(nb.pred != test$Gender) #0.4817475

##LDA/QDA

lda <- lda(Gender ~., train)
lda.pred <- predict(lda, test)

# misclassification rate
mean(lda.pred$class != test$Gender) #0.4554159

##AdaBoost (XGboost)

ada <- boosting(Gender ~., train)
ada.pred <- predict(ada, test)

# misclassification rate
mean(ada.pred$class != test$Gender) #0.4404548

#In this case, AdaBoost yields the lowest testing error.

#4.Are there classes that are relatively easy (or difficult) to predict?

table(ada.pred$class, test$Gender)

#accuracy:
#F: 123/(123+89+320)=0.231 and 123/(123+14+110)=0.498
#I: 441/(14+441+75)=0.832 and 441/(89+441+128)=0.670
#M: 371/(110+128+371)=0.609 and 371/(320+75+371)=0.484

#In conclusion, the class "I" is easier to predict 
#(which has the highest accuracy).

###Decision Boundary
#5.Save  a  new  training  dataset  (a  subset  of  the  training  data)  consisting  of  the  above  threecolumns.

train.new <- train[,c("Length", "Rings", "Gender")]

#6.Let’s use ‘Length’ as x-axis and ‘Rings’ as y-axis.
x <- seq(0, 1, by = 0.01)
y <- seq(0, 30, by = 0.3)
grid <- expand.grid(x, y)

#7.Create a new testing set consisting the (x, y) pairs we just created
test.new <- data.frame(Length = grid$Var1, Rings = grid$Var2)

#8.Using the new training data, build a LDA model.

qda2 <- qda(Gender ~ Length + Rings, train.new)
qda2.pred <- predict(qda2, test.new)

#9. Use all points in the ‘test.new’, make prediction.  Make a scatter plot to represent each point with a color according the prediction.

plot(x = test.new$Length, y = test.new$Rings, 
     xlab = "Length", ylab = "Rings",
     main = "LDA: Abalone Prediction",
     cex = 0.1,
     col = as.integer(qda2.pred$class)
     )
legend("topright",
       legend = c("Female", "Infant", "Male"),
       col = c(1, 2, 3),
       pch = c(19, 19, 19))
