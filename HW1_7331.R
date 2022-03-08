#Read data
Price <- read.csv("/Users/jason13nn/Desktop/SMU/Fall 2020/CS 7331/data/data_hw1.csv")

#transform X
X <- Price[,1:3]
X <- scale(X)
#append 1s
X <- cbind(intercept=c(rep(1,10)),X)
Y <- Price[,4]

#1
lm(Y ~ X, Price)

#The estimated equation is
#y = 20.3 + 6.8964*room - 2.2128*age + 0.2653*tax


#2
#calibFit package is no longer available for R 

#3
#Choose the initial point β0 as a vector of all 0.
beta_0 <- as.vector(rep(0,1000))
#set the step size α= 0.0005.
alpha <- 0.0005

for (k in 1:1000) {
  beta_k <- beta_0 - alpha * k
}
