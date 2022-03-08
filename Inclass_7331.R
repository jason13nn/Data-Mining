
#1. load data
load("/Users/jason13nn/Desktop/SMU/Fall 2020/CS 7331/data/rain_small.RData")

#2. head()
head(rain)

#3. remove NAs
rain <- na.omit(rain)
dim(rain) #105750 obs remaining

#4. duplicated()
dupTF <- duplicated(rain) #return T/F
which(dupTF) #print index if T

#5. Histogram
hist(rain$Expected, xlab = "Rainfall", ylab = "Frequency",
     main = "Histogram of Rainfall",
     ylim = c(0,1000))
summary(rain$Expected)
quantile(rain$Expected)

#6. Remove outliers
rain.outliers <- which(rain$Expected > 305)
rain2 <- rain[-rain.outliers,]
hist(rain2$Expected)

#7. Aggregate rows for gauge
aggregate(.~ Id, #aggregate each col per ID
          data = rain2, FUN = median)

#8. Covert continuous to categorical
rain_type <- cut(rain2$Expected, c(0, 4, 10, 305),
                 labels = c("Moderate", "Heavy", "Violent"))
rain2$Expected[1:5]
rain_type[1:5]

#9. Bar chart
group <- table(rain_type)
barplot(group, main = "Frequency per Rain Type",
        col = "salmon")

#10. Box-plot
boxplot(rain2$Expected ~ rain_type, 
        ylim = c(0, 60),
        xlab = "Rain Type",
        main = "Box Plot for each Rain Type"
        )
