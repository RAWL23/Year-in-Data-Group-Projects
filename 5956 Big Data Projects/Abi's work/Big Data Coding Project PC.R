#Install required packages

library(ggplot2)
library(caret)
library(dplyr)
library(tidyverse)
library(hexbin)

#Create an object of the dataset provided
data<-read.csv("IntOrg_NCD_variables_2022_02_02.csv")

#Create an object of the dataset with missing values removed
data<-na.omit(data)

#Plot a bar graph showing the total observations within each Superregion
ggplot(data = data) + theme(legend.position = "none") +
  geom_bar(mapping = aes(x = Superregion, fill = Superregion)) +
  coord_flip() +
  labs(title = "Population Sample in Superregion", x = "Superregion", y = "Population Sample")

#Plot a frequency polygon graph showing the mean BMI of children divided by their sex.
ggplot(data = data, mapping = aes(x = Mean_BMI_children, colour = Sex)) +
  geom_freqpoly(binwidth = 0.1)

#Plot an identical frequency polygon graph for mean BMI of adults by sex.
ggplot(data = data, mapping = aes(x = Mean_BMI_children, colour = Sex)) +
  geom_freqpoly(binwidth = 0.1) +
  labs(title = "Freqpoly of Mean BMI in Children by Sex", x = "Mean BMI in Children", y = "Count", colour = "Sex")

#Plot a frequency polygon graph showing mean BMI of children by Superregion.
ggplot(data = data, mapping = aes(x = Mean_BMI_adults, colour = Sex)) +
  geom_freqpoly(binwidth = 0.1) +
  labs(title = "Freqpoly of Mean BMI in Adults by Sex", x = "Mean BMI in Adults", y = "Count", colour = "Sex")

#Plot a frequency polygon graph showing mean BMI of adults by Superregion.
ggplot(data = data, mapping = aes(x = Mean_BMI_children, colour = Superregion)) +
  geom_freqpoly(binwidth = 0.1) +
  labs(title = "Freqpoly of Mean BMI in Children by Superregion", x = "Mean BMI Children", y = "Count", colour = "Superregion")

#Plot a histogram showing the overall number of observations at each bin of the BMI scale and each bin being divided by Superregion - different visualisation of previous freqpoly.
ggplot(data) +
  geom_histogram(mapping = aes(x = Mean_BMI_children, fill = Superregion), binwidth = 0.5) +
  labs(title = "Bargraph of Mean BMI in Children by Superregion", x = "Mean BMI in Children", y = "Count", colour = "Superregion")

#Plot a frequency polygon graph showing BMI of adults by Superregion.
ggplot(data = data, mapping = aes(x = Mean_BMI_adults, colour = Superregion)) +
  geom_freqpoly(binwidth = 0.1) +
  labs(title = "Frequency Polygon Graph of Mean BMI in Adults by Superegion", x = "Mean BMI in Adults", y = "Count", colour = "Superregion")

#Plot a histogram showing number of observations at each bin of the BMI scale, each bin divided by Superregion - again, a different visualisation of previous freqpoly.
ggplot(data) +
  geom_histogram(mapping = aes(x = Mean_BMI_adults, fill = Superregion), binwidth = 0.5) +  
  labs(title = "Bargraph of Mean BMI in Adults by Superregion", x = "Mean BMI Adults", y = "Count", colour = "Superregion")

#Plot a boxplot of mean BMI of children divided by sex to show the medians and outliers more clearly.
ggplot(data = data, mapping = aes(x = Sex, y = Mean_BMI_children)) +
  geom_boxplot() +
  labs(title = "Mean BMI Children by Sex", x = "Sex", y = "Mean BMI Children")

#Plot a histogram of the same data just presented in the boxplot (mean BMI of children by sex) - shows value of outliers more clearly.
ggplot(data) +
  geom_histogram(mapping = aes(x = Mean_BMI_children), binwidth = 0.1) +
  coord_cartesian(ylim = c(0,50)) +
  labs(title = "Mean BMI Children", x = "Mean BMI Children", y = "Count")

#Plot a boxplot of mean BMI of adults divided by sex to, again, show medians and outliers more clearly.
ggplot(data = data, mapping = aes(x = Sex, y = Mean_BMI_adults)) +
  geom_boxplot() + 
  labs(title = "Mean BMI adults by Sex", x = "Sex", y = "Mean BMI Adults")

#Plot a histogram of the same data just presented in the boxplot (Mean BMI adults by sex) - shows values of outliers more clearly.
ggplot(data) +
  geom_histogram(mapping = aes(x = Mean_BMI_adults), binwidth = 0.1) +
  coord_cartesian(ylim = c(0,50)) +
  labs(title = "Mean BMI in Adults", x = "Mean BMI Adults", y = "Count")

#Plot a frequency polygon graph of diabetes prevalence in males and females.
ggplot(data = data, mapping = aes(x = Diabetes_prevalence, colour= Sex)) +
  geom_freqpoly(binwidth = 0.01) +
  labs(title = "Diabetes Prevalence in Sex", x = "Diabetes Prevalence", y = "Count", colour = "Sex")

#Plot a frequency polygon of diabetes prevalence by Superregion
ggplot(data = data, mapping = aes(x = Diabetes_prevalence, colour = Superregion)) +
  geom_freqpoly(binwidth = 0.01) +
  labs(title = "Diabetes Prevalence per Superregion", x = "Superregion", y = "Count", colour = "Diabetes Prevelance")

#Plot a boxplot of diabetes prevalence by Superregion ordered by medians to clearly see medians and outliers.
ggplot(data = data) +
  geom_boxplot(
    mapping = aes(
      x = reorder(Superregion, Diabetes_prevalence, FUN = median),
      y = Diabetes_prevalence
    )) + 
  coord_flip() +
  labs(title = "Diabetes Prevalence per Superregion", x = "Superregion", y = "Diabetes Prevalence", colour = "Diabetes_prevelance")

#Count values within the variables of sex and superregion
data %>%
  count(Superregion, Sex)

#Plot a heatmap of the interaction between mean BMI of children and diabetes prevalence 
ggplot(data = data) +
  geom_hex(mapping = aes(x = Mean_BMI_children, y = Diabetes_prevalence))  +
  labs(title = "Heatmap of Mean BMI in Children and Diabetes Prevalence", x = "Mean BMI in Children", y = "Diabetes Prevalence", colour = "Count")

#Plot a heatmap of the interaction between mean BMI of adults and diabetets prevalence
ggplot(data) +
  geom_hex(mapping = aes(x = Mean_BMI_adults, y = Diabetes_prevalence)) +
  labs(title = "Heatmap of Mean BMI in Adults and Diabetes Prevalence", x = "Mean BMI in Adults", y = "Diabetes Prevalence", colour = "Count")

#Plot a boxplot of the interaction between mean BMI in children and diabetes prevalence in order to see median and outliers clearly.
ggplot(data = data, mapping = aes(x = Mean_BMI_children, y = Diabetes_prevalence)) +
  geom_boxplot(mapping = aes(group = cut_width(Mean_BMI_children, 0.5))) +
  labs(title = "Boxplot of Mean BMI in Children and Diabetes Prevalence", x = "Mean BMI in Children", y = "Diabetes Prevalence")

#Plot a boxplot of the interaction between mean BMI in adults and diabetes prevalence in order to see medians and outliers clearly
ggplot(data = data, mapping = aes(x = Mean_BMI_adults, y = Diabetes_prevalence)) +
  geom_boxplot(mapping = aes(group = cut_width(Mean_BMI_adults, 0.5))) +
  labs(title = "Boxplot of Mean BMI in Adults and Diabetes Prevalence", x = "Mean BMI in Adults", y = "Diabetes Prevalence")
