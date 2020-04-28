#This project will be exploring publicly available data from LendingClub.com. 
#Lending Club connects people who need money (borrowers) with people who have money (investors). 
#Hopefully, as an investor we would want to invest in people who showed a profile of having a 
#high probability of paying us back. We will try to create a model that will help predict this
#using a Support Vector Machine (SVM) algorithm.

#Reading the dataset:
loans <- read.csv('/Users/jeshurunpaul/Desktop/loan_data.csv')

head(loans)

#Checking the structure and summary:

str(loans)

summary(loans)

#Converting the necessary columns to categorical;

loans$credit.policy <- factor(loans$credit.policy)

loans$inq.last.6mths <- factor(loans$inq.last.6mths)

loans$delinq.2yrs <- factor(loans$delinq.2yrs)

loans$pub.rec <- factor(loans$pub.rec)

loans$not.fully.paid <- factor(loans$not.fully.paid)

#Exploratory Data Anaylsis (EDA):

#Histogram for fico classified by not.fully.paid;

library(ggplot2)

pl <- ggplot(loans, aes(fico)) + geom_histogram(aes(fill = not.fully.paid), color = 'black', 
                                          bins =40, alpha =0.5)

pl <- pl + scale_fill_manual(values = c( 'green','red')) + theme_bw()

pl

#Barplot of purpose counts, classified by not.fully.paid:

pl1 <- ggplot(loans, aes(x= factor(purpose))) + geom_bar(aes(fill = not.fully.paid), position = 'dodge')

pl1 <- pl1 + theme_bw() + theme( axis.text.x = element_text(angle = 90, hjust = 1))

pl1

#Scatterplot for int.rate and fico classified by not.fully.paid:

ggplot(loans, aes(int.rate,fico)) + geom_point(aes(color=not.fully.paid),alpha=0.3) +theme_bw()

#Building the model:

#TRAIN TEST SPLIT:

library(caTools)

set.seed(101)

sample <- sample.split(loans$not.fully.paid, SplitRatio = 0.7)

train <- subset(loans, sample == TRUE)

test <- subset(loans, sample == FALSE)

#Support Vector Machine (SVM):

library(e1071)

model <- svm(not.fully.paid ~ ., data = train)

summary(model)

#Predictions:

predicted.values <- predict(model, test[1:13])

table(predicted.values, test$not.fully.paid)

#Since the model performs poorly hyperparameter tuning has to be done to overcome this:

tune.results <- tune(svm,train.x = not.fully.paid ~ ., data = train, kernel='radial',
                     ranges = list(cost = c(1,10), gamma = c(0.1,1)))


#Final model:

model <- svm(not.fully.paid ~ .,data=train,cost=10,gamma = 0.1)

predicted.values <- predict(model,test[1:13])

table(predicted.values,test$not.fully.paid)

