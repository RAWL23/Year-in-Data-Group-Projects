#Setting the working directory and loading in the data
setwd("C:/Users/apr24/OneDrive/Documents/DA/Big Data")
clean_data <- read.csv("~/DA/Big Data/GlobalHealthData.csv")
View(clean_data)

#Removing the NAs
clean_data <- na.omit(clean_data)


#loading the necessary packages for EDA. If error message occurs code 'install.packages("NAME OF LIBRARY")'
library(vtable)
library(ggplot2)

#Creating a summary table for the whole data set
st(clean_data)
#this shows the mean, standard deviation, number etc of all variables in the data set.



## The next codes plot EDA for the prevalence of a raised blood pressure.


#Plotting a histogram showing the frequency of the prevalence of raised blood pressure
ggplot(data = clean_data)+ geom_histogram(mapping = aes(x = Prevalence_raised_blood_pressure)) +ggtitle("A Histogram to show the prevalence of 
raised blood pressure")

```

#Plotting a graph to show the frequency of prevalence of a raised blood pressure between different super regions.
ggplot(data = clean_data, mapping = aes(x = Prevalence_raised_blood_pressure, colour = Superregion)) +
  geom_freqpoly(binwidth = 0.1)+labs(x= "Prevalence of raised blood pressure",title = "Prevalence of raised blood pressure between Superregions", y="Frequency")



#Plotting a box plot to show the distribution of prevalence of raised blood pressure between different super regions
ggplot() + geom_boxplot(aes(y = clean_data$Prevalence_raised_blood_pressure, x = clean_data$Superregion, fill = clean_data$Superregion)) + theme_minimal() + coord_flip()


#Plotting a graph to show the frequency of prevalence of a raised blood pressure between sexes.
ggplot(data = clean_data, mapping = aes(x = Prevalence_raised_blood_pressure, colour = Sex)) +
  geom_freqpoly(binwidth = 0.01)+labs(x= "Prevalence of raised blood pressure",title = "Prevalence of raised blood pressure between Sex", y="Frequency")

#Creating a box plot to show the frequency of prevalence of a raised blood pressure between sexes.
boxplot(Prevalence_raised_blood_pressure~Sex, data= clean_data, main="A Box plot showing prevalence of raised blood pressure between Sexes",
        ylab="prevalence of raised blood pressure", xlab ="Sex") 




# Creating a plot to show the correlation between years of education and prevalence of raised blood pressure.
plot(clean_data$Years_of_education,clean_data$Prevalence_raised_blood_pressure,
     xlab = "Years of Education", ylab = "Prevalence of Raised blood pressure",
     main = "Scatterplot to show the correlation between Year
     and prevalence of raised blood pressure")




#Plotting a scatter plot to present the correlation between urbanisation and prevalence of a raised blood pressure.
plot(clean_data$Prevalence_raised_blood_pressure,clean_data$Urbanisation,
     xlab = "Prevalence of Rasied Blood Pressure", ylab = "Years of Education",
     main = "Scatterplot to show the correlation between Urbanisation
     and prevalence of raised blood pressure")

#Creating a graph to show the correlation between prevalence of raised blood pressure and GDP_USD
plot(clean_data$GDP_USD,clean_data$Prevalence_raised_blood_pressure,
     xlab = "GDP_USD", ylab = "Prevalence of raised blood pressure",
     main = "Scatterplot to show the
     correlation between GDP_USA
     and prevalence of raised blood pressure")


#Creating a graph to show the correlation between western diet score and Prevalence of a raised blood pressure
plot(clean_data$Western_diet_score,clean_data$Prevalence_raised_blood_pressure,
     xlab = "Western Diet Score", ylab = "Prevalence of raised blood pressure",
     main = "Scatterplot to show the correlation between 
     Western Diet score and prevalence of raised blood pressure") 


## Systolic Blood Pressure

#Plotting a histogram to show the frequency of systolic blood pressure 

ggplot(data = clean_data)+ geom_histogram(mapping = aes(x = Systolic_blood_pressure)) +ggtitle("A Histogram to show the distribution of Systolic blood pressure")
#This plot presents that the most common systolic blood pressure is between 136-129 and then gradually decreases after this. 

#Plotting  a graph to show the systolic blood pressure between different super regions
ggplot(data = clean_data, mapping = aes(x = Systolic_blood_pressure, colour = Superregion)) +
  geom_freqpoly(binwidth = 0.5)+labs(x= "Systolic blood pressure",title = "Systolic blood pressure between different Superregions", y="Frequency")

#Plotting a box plot to show the distribution of systolic blood pressure between different super regions
ggplot() + geom_boxplot(aes(y = clean_data$Systolic_blood_pressure, x = clean_data$Superregion, fill = clean_data$Superregion)) + theme_minimal() + coord_flip()

#Plotting a graph to show systolic blood pressure between sexes
ggplot(data = clean_data, mapping = aes(x = Systolic_blood_pressure, colour = Sex)) +
  geom_freqpoly(binwidth = 0.1)+labs(x= "Systolic Blood Pressure",title = " A graph to show Systolic blood pressure between Sex", y="Frequency") 

#Plotting a graph to show the distribution of systolic blood pressure between sexes.
boxplot(Systolic_blood_pressure~Sex, data= clean_data, main="A Box plot showing Systolic Blood Pressure between Sexes",
        ylab ="Systolic Blood Pressure", xlab ="Sex") 

#Plotting a graph to show the correlation between Urbanisation and systolic blood pressure
plot(clean_data$Systolic_blood_pressure,clean_data$Urbanisation,
     xlab = "Systolic Blood Pressure", ylab = "Urbanisation",
     main = "Scatterplot to show the correlation between years of 
     education and systolic blood pressure")

#Plotting a graph to show the correlation between systolic blood pressure and years of education
plot(clean_data$Years_of_education,clean_data$Systolic_blood_pressure,
     xlab = "Years of Education", ylab = "Systolic Blood Pressure",
     main = "Scatterplot to show the correlation between years of 
     education and Systolic blood pressure")


#Plotting a graph to show the correlation between Western Diet score and Systolic blood pressure
plot(clean_data$Western_diet_score,clean_data$Systolic_blood_pressure,
     xlab = "Western Diet Score", ylab = "Systolic Blood Pressure",
     main = "Scatterplot to show the correlation between
     western diet score and Systolic Blood Pressure")


#Plotting a graph to show the correlation between Systolic blood pressure and GDP/USD
plot(clean_data$GDP_USD,clean_data$Systolic_blood_pressure,
     xlab = "gdp/usd", ylab = " Systolic Blood Pressure",
     main = "Scatterplot to show the correlation
     between GDP and USD and Systolic blood pressure")

