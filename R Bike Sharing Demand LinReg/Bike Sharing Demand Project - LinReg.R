#This Project aims at predicting the count of bike rentals for a particular time period
#across various seasons. This is a kaggle challenge and the link is attached below
# Link: https://www.kaggle.com/c/bike-sharing-demand/data

#Reading the CSV file:
bike <- read.csv('/Users/jeshurunpaul/Desktop/bikeshare.csv')
head(bike)

#Scatterplot between count and temperature:
library(ggplot2)
ggplot(bike, aes(temp,count)) + geom_point(aes(color=temp),alpha = 0.3) + theme_bw()

#Scatterplot between count and datetime:
#The datetime is converted to POSIXct format before plotting:
bike$datetime <- as.POSIXct(bike$datetime)
pl <- ggplot(bike, aes(datetime,count)) + geom_point(aes(color=temp),alpha = 0.3) 
pl2 <- pl+ scale_color_continuous(low = 'blue', high = 'orange')+ theme_bw()
pl2

#Correlation between temperature and count:
cor(bike[,c('temp','count')])

#Boxplot for different seasons for their count:
ggplot(bike,aes(factor(season),count)) + geom_boxplot(aes(color = factor(season))) + theme_bw()

#Feature Engineering:
bike$hour <- sapply(bike$datetime, function(x) { format(x, '%H')})

#Scatterplot between hour and count for the Working Days:
library(dplyr)
pl <- ggplot(filter(bike, workingday ==1), aes(hour,count)) 
pl <- pl + geom_point( position = position_jitter(w=1,h=0),
                                                  aes(color = temp), alpha = 0.5) + theme_bw()
pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue',
                                             'light green','yellow','orange','red'))
pl


#Scatterplot between hour and count for the Non Working Days:
pl1 <- ggplot(filter(bike, workingday ==0), aes(hour,count)) 
pl1 <- pl1 + geom_point( position = position_jitter(w=1,h=0),
                       aes(color = temp), alpha = 0.5) + theme_bw()
pl1 <- pl1 + scale_color_gradientn(colours = c('dark blue','blue','light blue',
                                             'light green','yellow','orange','red'))
pl1

#Building a linear regression model to predict the count solely based on temperature:
temp.model <- lm(count ~ temp, bike)
summary(temp.model)
 
#Building the main model:
bike$hour <-sapply(bike$hour,as.numeric)

model <- lm(count ~ .  -casual - registered -datetime -atemp, bike)
summary(model)

