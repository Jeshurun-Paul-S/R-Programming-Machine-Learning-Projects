#This project will be attempting to predict if people in the data set belong in a certain class by salary, 
#either making <=50k or >50k per year using Logistic Regression.

#Reading the CSV File:
adult <- read.csv('/Users/jeshurunpaul/Desktop/adult_sal.csv')
head(adult)

#Removing the additional index:
library('dplyr')
adult <- select(adult, -X)

#Observing the structure of the data:
str(adult)

#Observing the summary of the data:
summary(adult)

#Checking the frequency of the type_employer column:
table(adult$type_employer)

#Combining the never worked and the without pay category under Unemployed:

unemp <- function(job){
  job <- as.character(job)
  if (job == 'Never-worked' | job == 'Without-pay') {
    return('Unemployed')
  }
  else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer, unemp)

#Combining State and Local gov jobs into a category called SL-gov and combininig self-employed jobs 
#into a category called self-emp:

emp <- function(job){
  job <- as.character(job)
  if (job == "Local-gov" | job == "State-gov") {
    return('SL-gov')
  }
  else if (job == 'Self-emp-inc' | job == 'Self-emp-not-inc') {
    return('self-emp')
  } 
    else {
      return(job)
    }
    
  }
  

adult$type_employer <- sapply(adult$type_employer, emp)

#Marital column lookup:
table(adult$marital)

#Reducing Marital to three groups:Married, Not-Married ,Never-Married:
mar <- function(stat) {
  stat <- as.character(stat)
  if (stat == 'Married-AF-spouse' | stat == 'Married-civ-spouse'  | stat == 'Married-spouse-absent') {
    return("Married")
  }
  else if (stat == 'Divorced' | stat == 'Separated' | stat == 'Widowed') {
    return("Not-Married")
  }
  else{
    return(stat)
  }
  
}

adult$marital <- sapply(adult$marital, mar)

#Looking up Country column:

table(adult$country)

levels(adult$country)

#Assigning all the countries to some sections:
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(coun){
  if (coun %in% Asia) {
    return("Asia")
  }
  else if (coun %in% North.America) {
    return("North.America")
  }
  else if (coun %in% Europe) {
    return("Europe") 
  }
  else if (coun %in% Latin.and.South.America) {
    return("Latin.and.South.America") 
  }
  else {
    return("Other") 
  }
  
}

adult$country <- sapply(adult$country, group_country)

#Checking the structure again:
str(adult)

#Converting Character columns to factor:

library(Amelia)

adult[adult == "?"] <- NA

adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
adult$occupation <- sapply(adult$occupation,factor)

table(adult$type_employer)

#Plotting Missingness map:
missmap(adult)

missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

#Removing null values:
adult <- na.omit(adult)

#Checking if alL the null values are dropped:
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

#Exploratory Data Analysis:
library(ggplot2)
library(dplyr)

#Plotting a histogram for age classified by income:
ggplot(adult,aes(age)) + geom_histogram(aes(fill = income), color = 'black',
                                        binwidth = 1) + theme_bw()

#Plotting a histogram for hours per week:
ggplot(adult, aes(hr_per_week)) + geom_histogram() + theme_bw()

#Renaming Country column to Region:
names(adult)[names(adult)=="country"] <- "region"

#Creating Barplot for Region classified by income:
ggplot(adult, aes(region)) + geom_bar(aes(fill = income), color = 'black') + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Building Model using Logistic Regression:

library(caTools)

set.seed(101)

sample <- sample.split(adult$income, SplitRatio = 0.7)

train = subset(adult, sample ==TRUE)

test = subset(adult, sample == FALSE)


model = glm(income ~ ., family = binomial(logit), data = train)

summary(model)

test$predicted.income <- predict(model,newdata = test, type = 'response')

table(test$income,test$predicted.income > 0.5)

#Accuracy of the model:
(6372+1423)/(6372+1423+548+872)

#recall
6732/(6372+548)

#precision
6732/(6372+872)

