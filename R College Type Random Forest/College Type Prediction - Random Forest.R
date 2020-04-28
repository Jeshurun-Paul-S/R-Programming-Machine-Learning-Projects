#This project aims at predicting whether or not a particular college is private
#based on several features with the help of a decision tree method.


#Importing the Dataset from ISLR:
library(ISLR)

head(College)

df <- College

#Exploratory Data Analysis (EDA): 

library(ggplot2)

#PLottting a scatterplot between Room.Board and Grad.Rate, classified based on Private Status

ggplot(df, aes(Room.Board,Grad.Rate)) + geom_point(aes(color = Private)) + theme_bw()

#Histogram of fulltime students classified by Private status:

ggplot(df,aes(F.Undergrad)) + geom_histogram(aes(fill = Private), color = 'black', bins =50) 

#Histogram of Grad.Rate classified by Private Status:

ggplot(df, aes(Grad.Rate)) + geom_histogram(aes(fill = Private),color = 'black', bins=50)

#Graduation Rate should not be greater than 100, hence taking care of the outlier:

subset(df, Grad.Rate>100)

df["Cazenovia College","Grad.Rate"] <- 100

#TRAIN TEST SPLIT:

library(caTools)

set.seed(101)

sample <- sample.split(df$Private, SplitRatio = 0.7)

train <- subset(df,sample == TRUE)

test <- subset(df,sample == FALSE)

library(randomForest)

model <- randomForest(Private ~ ., data = train ,importance = TRUE)

#Confusion Matrix:

model$confusion

#Importance:

model$importance

#Predictions:

preds <- predict(model,test)

table(preds, test$Private)

#Accuracy

(57+163) / (57+6+7+163)

