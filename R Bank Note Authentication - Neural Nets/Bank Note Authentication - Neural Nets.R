#This project aims at predicting whether or not a particular bank note is original
#This can be predicted from the features with the help of Neural Nets Algorithm.  

#Reading the dataset:

df <- read.csv('/Users/jeshurunpaul/Desktop/bank_note_data.csv')

#Checking the Structure of the data:

str(df)

#Since the data is small we dont necessarily need to perform Exploratory Data Analysis,
#because the data isn't easily interpretable since its just statistical info on images.

#TRAIN TEST SPLIT:

library(caTools)

set.seed(101)

sample <- sample.split(df$Class, SplitRatio = 0.7)

train <- subset(df, sample == TRUE)

test <- subset(df, sample == FALSE)

#NEURAL NETS:

library(neuralnet)

nn <- neuralnet(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy,
                data=train,hidden=10,linear.output=FALSE)

predicted.nn.values <- compute(nn,test[,1:4])

head(predicted.nn.values$net.result)

predictions <- sapply(predicted.nn.values$net.result, round)

head(predictions)

table(predictions, test$Class)

#Since we appear to be getting an 100 percent accuracy, its suspicious and hence we could 
#check it with Random Forest algorithm.

library(randomForest)

df$Class <- factor(df$Class)

set.seed(101)

split = sample.split(df$Class, SplitRatio = 0.70)

train = subset(df, split == TRUE)

test = subset(df, split == FALSE)

#Random Forest Model:

model <- randomForest(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy,data=train)

rf.preds <- predict(model,test)

table(rf.preds, test$Class)

#Since we appear to get 99.2 percent accuracy in random forest it is acceptable that 
#the neural nets gave us 100 percent accuracy 

