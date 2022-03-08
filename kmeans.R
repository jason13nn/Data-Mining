data("USArrests")
?USArrests
myData <- USArrests

# K-means
K <- 3
maxIter <- 10
set.seed(0)
km <- kmeans(myData, K, maxIter)
km$cluster
km$centers
plot(myData[ ,1], myData[ ,2], col = km$cluster)

# 3-d scatter plot
install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(myData[ ,1:3], color = km$cluster)

# hierarchical clustering
d <- dist(myData, method = "euclidean")
hc <- hclust(d, method = "centroid")
plot(hc, cex = 0.5)
rect.hclust(hc, k=2)
hc_label <- cutree(hc, k=2)
hc_label[1:5]



