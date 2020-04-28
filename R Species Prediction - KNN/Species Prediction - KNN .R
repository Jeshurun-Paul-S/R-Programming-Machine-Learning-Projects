#This project aims at predicting the species of the flowers from the iris dataset using the
#K - Nearest Neighbours (KNN) algorithm, depending upon the various features.

# Importing the dataset:
library(ISLR)
head(iris)

#Understanding the Structure of the dataset:
str(iris)

#Scaling the data:
scaled.iris <- scale(iris[1:4])

#Checking the variance to see if the scaling worked well:
var(scaled.iris[,1])

#Joining the target column:
final.data <- cbind(scaled.iris,iris[5])
head(final.data)

#TRAIN TEST SPLIT:
set.seed(101)

library(caTools)

sample <- sample.split(final.data$Species, SplitRatio = 0.7)

train <- subset(final.data, sample == TRUE)

test <- subset(final.data, sample == FALSE)

#Building the KNN Model
library(class)

predicted.species <- knn(train[1:4], test[1:4], train$Species, k=1)

predicted.species


#Misclassification Rate:
mean(test$Species != predicted.species)

#Choosing a K value;
predicted.species <- NULL
error.rate <- NULL
for (i in 1:10) {
  set.seed(101)
  predicted.species <- knn(train[1:4], test[1:4], train$Species, k=i)
  error.rate[i] <- mean( test$Species != predicted.species)
}

#Plotting an Elbow plot:
library(ggplot2)

k.values <- 1:10

error.df <- data.frame( k.values, error.rate)

pl <- ggplot(error.df, aes(x=k.values, y=error.rate)) + geom_point()

pl<- pl + geom_line(lty = "dotted", color = 'red')

pl

#Since the error rate is expected to be minimum it is reasonable to choose the k-value as 3

final.predicted.species <- knn(train[1:4], test[1:4], train$Species, k=3)

final.predicted.species

#Misclassification rate:

mean( test$Species != final.predicted.species)

