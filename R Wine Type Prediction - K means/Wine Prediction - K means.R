#This project aims at predicting the type of wine based on several features of wine.
#the classification will be to seperate the red and white wines and predict a wine's type
#given its features. This project uses K - means Clustering algorithm to make the predictions.


#While dealing with an unsupervised learning problem, its difficult to get a good measure 
#of how well the model performed. For this project, we will use data from the UCI archive based off of
#red and white wines.We will then add a label to a combined data set, we'll bring this label 
#back later to see how well we can cluster the wine into groups.

# Reading the data:

df1 <- read.csv('/Users/jeshurunpaul/Desktop/winequality-red.csv')

df2 <- read.csv('/Users/jeshurunpaul/Desktop/winequality-white.csv')

#Adding Labels:

df1$label <- sapply(df1$pH, function(x) {'red'})

df2$label <- sapply(df2$pH, function(x) {'white'})

#Combining the datasets:

wine <- rbind(df1,df2)

str(wine)

#Exploratory Data Analysis:

#Histogram for residual.sugar classified by the label:

library(ggplot2)

pl <- ggplot(wine, aes(residual.sugar)) + geom_histogram(aes(fill = label), color = 'black', bins =50)

pl <- pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_bw()

pl

#Histogram for citric.acid classified by the label:

pl1 <- ggplot(wine, aes(citric.acid)) + geom_histogram(aes(fill = label), color = 'black', bins =50)

pl1 <- pl1 + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_bw()

pl1

#Histogram for alcohol classified by the label:

pl2 <- ggplot(wine, aes(alcohol)) + geom_histogram(aes(fill = label), color = 'black', bins =50)

pl2 <- pl2 + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_bw()

pl2


#Scatterplot between  citric.acid and residual.sugar classified by the label:

pl3 <- ggplot(wine, aes(x=citric.acid, y=residual.sugar)) + geom_point(aes(color = label), alpha = 0.2)

pl3 <- pl3 + scale_color_manual(values = c('#ae4554','#faf7ea')) + theme_dark()

pl3

#Scatterplot between  volatile.acidity and residual.sugar classified by the label:

pl4 <- ggplot(wine, aes(x=volatile.acidity, y=residual.sugar)) + geom_point(aes(color = label), alpha = 0.2)

pl4 <- pl4 + scale_color_manual(values = c('#ae4554','#faf7ea')) + theme_dark()

pl4

#Wine data without label:

clus.data <- wine[, 1:12]

#Wine Cluster:

wine.cluster <- kmeans(wine[1:12], 2)

wine.cluster$centers

#Confusion Matrix:

#Eventhough we dont have the luxury of labelled data in Unsupervised learning, 
#we are just checking the predictions

table(wine$label,wine.cluster$cluster)

