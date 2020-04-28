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

#BUILDING THE MODEL:

library(rpart)

tree <- rpart(Private ~ ., method = 'class', data = train)

#Predictions:

tree.preds <- predict(tree,test)

head(tree.preds)

tree.preds <- as.data.frame(tree.preds)

joiner <- function(x){
  
  if (x >= 0.5) {
    return("Yes")
  }
  else {
    return("No")
  }
}

 tree.preds$Private <- sapply(tree.preds$Yes, joiner)
 
 head(tree.preds)
 
#Confusion Matrix;
 
table(tree.preds$Private, test$Private)

#Acuracy

(57+160)/(57+9+7+160)

#Decision Tree;

library(rpart.plot)

prp(tree)

