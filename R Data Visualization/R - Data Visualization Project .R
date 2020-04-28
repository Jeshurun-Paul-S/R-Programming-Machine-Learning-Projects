#This project aims at recreating a plot from The Economist Magazine which explains the 
#relationship between the Corruption Perception Index (CPI) and the Human Development Index (HDI) 
#in several countries of the world. The article is available in the link attached
#https://www.economist.com/graphic-detail/2011/12/02/corrosive-corruption
#(Cntrl + Click to open the link)

# Necessary Libraries
library(ggplot2)
library(ggthemes)
library(data.table)

#Reading the dataframe
df <- fread('/Users/jeshurunpaul/Desktop/Economist_Assignment_Data.csv')
df

#Creating a basic scatter
pl <- ggplot(df, aes(x=CPI,y=HDI,color = Region)) + geom_point(size=4,shape=1)


# Adding a smooth fit line
pl2 <- pl + geom_smooth(aes(group=1),method = 'lm',formula = y~log(x),se = FALSE,color = 'red')


#Adding labels to the points

#Since there are many countries displaying the names of all of those would be messy
#Hence I have selected a subset of countries as similar to the Economist plot.

pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")

pl3 <- pl2 + geom_text(aes(label = Country), color = "gray20", 
                       data = subset(df, Country %in% pointsToLabel),check_overlap = TRUE)



pl4 <- pl3 + theme_bw()


# Adding the X - label
pl5 <- pl4 + scale_x_continuous(name = 'Corruption Perceptions Index, 2011 (10=least corrupt)',
                          limits = c(0.9,10.5), breaks = 1:10)


#Adding the Y - label
pl6 <- pl5 + scale_y_continuous(name = 'Human Development Index, 2011 (1=Best)',
                                limits = c(0.2,1.0))


#Adding the theme
pl7 <- pl6 + theme_economist_white()
pl7

