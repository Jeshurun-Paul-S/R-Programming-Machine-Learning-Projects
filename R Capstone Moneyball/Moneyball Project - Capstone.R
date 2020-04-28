#In this project the goal is to find replacement players for the Oakland A Basketball Team,
# the ones lost at the start of the off-season.During the 2001–02 offseason, the team lost three key 
#free agents to larger market teams: 2000 AL MVP Jason Giambi to the New York Yankees, outfielder 
#Johnny Damon to the Boston Red Sox, and closer Jason Isringhausen to the St. Louis Cardinals.

#Reading the csv file
batting <- read.csv('/Users/jeshurunpaul/Desktop/Batting.csv')

#Checking the Structure:
str(batting)

#Computing batting average
batting$BA <- batting$H / batting$AB

#Computing On Base percentage:
batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + 
                                                         batting$HBP + batting$SF)

#Computing the singles:
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR

#Computing Slugging Average:
batting$SLG <- ((1 * batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR) ) / batting$AB
tail(batting,5)

#Checking the Structure:
str(batting)

#Reading the Salary data:
sal <- read.csv('/Users/jeshurunpaul/Desktop/Salaries.csv')

#Checking the summary of salaries and batting:
summary(sal)
summary(batting)

#We observe that the batting data goes way back till 1871, its obvious to select a subset starting from 1985

batting <- subset(batting, yearID>=1985)
summary(batting)

#Merging both the batting data and the salary data:
combo <- merge(batting,sal,by = c('playerID','yearID'))
summary(combo)

#Analysing the lost players:
#We know that the lost players are Jason Giambi (giambja01,Johnny Damon (damonjo01),
#Rainer Gustavo "Ray" Olmedo ('saenzol01').
lost_players <- subset(combo, playerID %in% c('giambja01','damonjo01','saenzol01') )
lost_players

#It is reasonable to use the data for year 2001 alone:
lost_players <- subset(lost_players, yearID == 2001)
lost_players <- select(lost_players,playerID,H,X2B,X3B,HR,OBP,SLG,BA,AB)
lost_players

#Now the task is to find the replacement players:
avail_players <- filter(combo, yearID == 2001)

#Plotting the graph of OBP vs salary
library(ggplot2)
ggplot(avail_players, aes(x=OBP,y=salary)) + geom_point()

#By looking at the data it is fairly reasonable to choose the players with salary less than 8000000 and
# to leave out the ones with zero OBP

avail_players <- filter(avail_players,salary<8000000,OBP>0)

#The total AB of the lost players is 1469. This is about 1500, meaning we should probably cut off 
#our avail_players at 1500/3= 500 AB probably cut off my avail.players at 1500/3= 500 AB

avail_players <- filter (avail_players, AB >= 500)

#The possible players for replacement are of the order:
possible <- head(arrange(avail_players, desc(OBP)),10)
possible

#The columns of interest are selected:
possible <- possible[, c('playerID','OBP','AB','salary')]
possible

#Since we cannot choose giambja01 again as he had left we have to choosenthe next three players:
possible[2:4,]

#Hence heltoto01, berkmla01 and  gonzalu01 should be the players in replacement for
#'giambja01','damonjo01' and 'saenzol01' for Oakland A.


