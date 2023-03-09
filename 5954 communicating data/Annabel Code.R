
#Setting the working directory 
setwd("C:/Users/apr24/OneDrive/Documents/DA/PACD")


#Importing data and data cleaning
lifexpectancydata <- read.csv("~/DA/PACD/lifexpectancydata.csv", header=TRUE)
View(lifexpectancydata)
clean_lifedata <- na.omit(lifexpectancydata)


# Libraries
install.packages("hrbrthemes")
install.packages("viridis")
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(tidyverse)
library(dplyr)

# Create data frame
data <- data.frame(clean_lifedata)


# bubble plot to show life expectancy between GDP, Status and Adult Mortality 
ggplot(data, aes(x= GDP, y= Life_expectancy, size = Adult.Mortality, colour= Status)) + geom_point(alpha=0.7) + ggtitle("Life expectancy between status, GDP and adult mortality") + labs(x= "GDP (in USD)", y= "Life Expectancy (Age)") + theme(plot.title = element_text(size = 8, face = "bold")) + theme(plot.title.position = "plot")


# A graph to show distribution of BMI between developing and Developed Countries
ggplot(data, aes(x = BMI, y = Status, fill = Status)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none") +
  ggtitle(" A graph to show average BMI between developed and developing countries") +
  labs(xlab= "Average BMI", ylab= "Status") + scale_x_continuous(breaks= seq(0,80,5), sec.axis= dup_axis())  +
  scale_fill_brewer(palette="Pastel1") + theme(axis.text=element_text(size=8),
 axis.title=element_text(size=8,face="bold")) +
  theme(plot.title.position = "plot",plot.title = element_text(size = 8, face = "bold")) + xlab("Average BMI") + ylab("Country Status")

#A graph to show the distribution of Adult Mortality between Developing and Developed Countries
ggplot(data, aes(x = Adult.Mortality, y = Status, fill = Status)) +
  geom_density_ridges() +
  theme_ridges() + theme(legend.position = "none") +
  ggtitle("A graph to show the Distribution of Adult Mortality between developed and Developing countries") + labs(xlab= "Adult Mortality", ylab= "Country Status") +
  scale_x_continuous(breaks= seq(0,800,100), sec.axis= dup_axis())  +
  scale_fill_brewer(palette="Pastel2") + theme(axis.text=element_text(size=6),axis.title=element_text(size=6,face="bold")) +
  theme(plot.title = element_text(size = 6, face = "bold")) + xlab("Adult Mortality Rate") + ylab("Country Status") + theme(plot.title.position = "plot")
                                                                                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                                     
#A box plot to show the distribution of BMI between Developed and Developing Status
ggplot(data, aes(x=Status, y=BMI, fill=Status)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.5, alpha=0.9) +
  theme_ipsum() +
  theme(plot.title.position = "plot",
    legend.position="none",
    plot.title = element_text(size=8)
  ) + scale_fill_brewer(palette="RdPu") + ggtitle(" A box plot to show the distribution of mean BMI between developed and developing countries") + labs(x= "Country Status", y= " Average BMI", hjust= 0.5) 



