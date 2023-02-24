##### DATA INPUT ###############################################################
##### The data file must be a csv file with the following columns:             
##### DATA CHARACTERISTICS
##### Country: country names, which must match covariate file
##### ISO: country code 
##### sex: must be either "male" or "female", as the two sexes are modelled    
##### separately  
##### Year: Year number
##### mean_bmi_children: positive number 
##### Prevalence_obesity_children : positive decimal
##### Prevalence_overweight_children : positive decimal
##### Prevalence_underweight_children : positive decimal
##### mean_bmi_adults: positive number                                                      
##### Prevalence_obesity_adults : positive decimal
##### Prevalence_underweight_adults : positive decimal
##### Prevalence_morbid_obesity_adults : positive decimal
##### Diabetes_prevalence : positive decimal
##### Systolic_blood_pressure : positive decimal
##### Prevalence_rasied_blood_pressure : postive decimal
##### Region: region names, which must match covariate file                    
##### Superregion: superregion names, which must match covariate file 
##### Years_of_education : positive decimal
##### Urbanisation : postive decimal
##### Western_diet_score : positive and negative decimals
##### GDP_USD : positive decimals
################################################################################

# librarys
# this will get any of the librarys needed to run the code, if any error occurs use 
# install.packages("NAME OF LIBRARY")
library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)
library(randomForest)
library(viridisLite)
library(GGally)
library(hexbin)
library(tidyverse)
library(vtable)

#Gathering Data
data <- read.csv("IntOrg_NCD_variables_2022_02_02.csv")

#Create an object of the dataset with missing values removed
clean_data<-na.omit(data)

---------------------------------------------------------------------------------------------------------------------------------------------------

#Creating a summary table for the whole data set after NAs are removed
st(clean_data)
#this shows the mean, standard deviation, number etc of all variables in the data set.
# The data consists of 50% males and 50% females
#There is 14000 data points in this data set
#Mean BMI for children is 18.625
#Mean BMI for adults is 24.62

## The next codes plot EDA for the prevalence of a raised blood pressure.


#Plotting a histogram showing the frequency in observations of the prevalence of raised blood pressure
ggplot(data = clean_data)+ geom_histogram(mapping = aes(x = Prevalence_raised_blood_pressure)) +ggtitle("A Histogram to show the prevalence of 
raised blood pressure")
# The number of observations of prevalence to high blood pressure is high around 0.25-0.35 and gradually decreases the higher the prevalence.

#Plotting a frequency polygon graph to show the frequency of prevalence of a raised blood pressure between different super regions.
ggplot(data = clean_data, mapping = aes(x = Prevalence_raised_blood_pressure, colour = Superregion)) +
  geom_freqpoly(binwidth = 0.1) + labs(x= "Prevalence of raised blood pressure",title = "Prevalence of raised blood pressure between Superregions", y="frequency")
#High-income western countries and central and eastern Europe have higher prevalence to a raised blood pressure. High income Asia Pacific can be seen to have the lowest.


#Plotting a box plot to show the distribution of prevalence of raised blood pressure between different super regions
ggplot() + geom_boxplot(aes(y = clean_data$Prevalence_raised_blood_pressure, x = clean_data$Superregion, fill = clean_data$Superregion)) + theme_minimal() +
  coord_flip() + labs(title = "A box plot to show the distribution of prevalence of raised blood pressure between super regions", x = "Super region", y = "Prevalence of raised blood pressure")
# The median for Central and Eastern Europe was the highest but High Income Western Countries has a higher maximum value.                                                                                                                                                                                    
#There are many outliers especially with Latin America and Caribbean  and Oceania

#Plotting a frequency polygon graph to show the prevalence of a raised blood pressure between sexes.
ggplot(data = clean_data, mapping = aes(x = Prevalence_raised_blood_pressure, colour = Sex)) +
  geom_freqpoly(binwidth = 0.01)+labs(x= "Prevalence of raised blood pressure",title = "Prevalence of raised blood pressure between Sex", y="Frequency")
#Males are more likely to have a higher prevalence of raised blood pressure than women.

#Creating a box plot to show the frequency of prevalence of a raised blood pressure between sexes.
boxplot(Prevalence_raised_blood_pressure~Sex, data= clean_data, main="A Box plot showing prevalence of raised blood pressure 
        between the Sexes",
        ylab="prevalence of raised blood pressure", xlab ="Sex") 
#Men are still more likely to have a raised blood pressure but there are some outliers that need to be considered when comparing these variables.

# Creating a plot to show the correlation between years of education and prevalence of raised blood pressure.
plot(clean_data$Years_of_education,clean_data$Prevalence_raised_blood_pressure,
     xlab = "Years of Education", ylab = "Prevalence of Raised blood pressure",
     main = "Scatterplot to show the correlation between years of education
     and prevalence of raised blood pressure")
#There is not much of a trend between years of education and prevalence of raised blood pressure. 


#Plotting a scatter plot to present the correlation between urbanisation and prevalence of a raised blood pressure.
plot(clean_data$Prevalence_raised_blood_pressure,clean_data$Urbanisation,
     xlab = "Prevalence of Rasied Blood Pressure", ylab = "Urbanisation",
     main = "Scatterplot to show the correlation between Urbanisation
     and prevalence of raised blood pressure")
#There is no trend or pattern between urbanisation and prevalence of raised blood pressure.

#Creating a graph to show the correlation between prevalence of raised blood pressure and GDP_USD
plot(clean_data$GDP_USD,clean_data$Prevalence_raised_blood_pressure,
     xlab = "GDP/USD", ylab = "Prevalence of raised blood pressure",
     main = "Scatterplot to show the
     correlation between GDP/USD
     and prevalence of raised blood pressure")
#There are no patterns or trends between GDP in USD and prevalence of raised blood pressure


#Creating a graph to show the correlation between western diet score and Prevalence of a raised blood pressure
plot(clean_data$Western_diet_score,clean_data$Prevalence_raised_blood_pressure,
     xlab = "Western Diet Score", ylab = "Prevalence of raised blood pressure",
     main = "Scatterplot to show the correlation between 
     Western Diet score and prevalence of raised blood pressure") 
#There does not seem to be much of a trend between western diet score and prevalence of raised blood pressure.

## Systolic Blood Pressure

#Plotting a histogram to show the frequency of systolic blood pressure 

ggplot(data = clean_data)+ geom_histogram(mapping = aes(x = Systolic_blood_pressure)) +ggtitle("A Histogram to show the distribution of Systolic blood pressure")
#This plot presents that the highest frequency of systolic blood pressure is between 126-129 and then gradually decreases after this. 

#Plotting a graph to show the systolic blood pressure between different super regions
ggplot(data = clean_data, mapping = aes(x = Systolic_blood_pressure, colour = Superregion)) +
  geom_freqpoly(binwidth = 0.5)+labs(x= "Systolic blood pressure",title = "Systolic blood pressure between different Superregions", y="frequency")
#Central Asia and North-Africa-Middle East have the highest count of higher systolic blood pressures as well as Sub-Saharan also mainly being on the higher end of the x-axis. 
#Oceania counts stay relatively similar from the lower blood pressure score to the higher scores.

#Plotting a box plot to show the distribution of systolic blood pressure between different super regions
ggplot() + geom_boxplot(aes(y = clean_data$Systolic_blood_pressure, x = clean_data$Superregion, fill = clean_data$Superregion)) + theme_minimal() + coord_flip() +
  labs(title = "A boxplot to show systolic
       blood pressure between super regions", x = "Superregion", y = "Systolic Blood Pressure")
#Central and Eastern Europe have the highest median systolic blood pressure and South Asia has the lowest median.
#There are many outliers, especially regarding Sub-Saharan Africa and Central Asia and North Africa-Middle East.

#Plotting a graph to show systolic blood pressure between sexes
ggplot(data = clean_data, mapping = aes(x = Systolic_blood_pressure, colour = Sex)) +
  geom_freqpoly(binwidth = 0.1)+labs(x= "Systolic Blood Pressure",title = " A graph to show Systolic blood pressure between Sex", y="frequency") 
#Men have a higher systolic  blood pressure than women.


#Plotting a graph to show the distribution of systolic blood pressure between sexes.
boxplot(Systolic_blood_pressure~Sex, data= clean_data, main="A Box plot showing Systolic Blood Pressure between Sexes",
        ylab ="Systolic Blood Pressure", xlab ="Sex") 
#Men have a higher median of systolic blood pressure in comparison to females
#There are a few outliers for women for lower systolic blood pressures.

#Plotting a graph to show the correlation between Urbanisation and systolic blood pressure
plot(clean_data$Systolic_blood_pressure,clean_data$Urbanisation,
     xlab = "Systolic Blood Pressure", ylab = "Urbanisation",
     main = "Scatterplot to show the correlation between years of 
     education and systolic blood pressure")
#There is no correlation between urbanisation and systolic blood pressure

#Plotting a graph to show the correlation between systolic blood pressure and years of education
plot(clean_data$Years_of_education,clean_data$Systolic_blood_pressure,
     xlab = "Years of Education", ylab = "Systolic Blood Pressure",
     main = "Scatterplot to show the correlation between years of 
     education and Systolic blood pressure")
#There is no correlation between years of education and systolic blood pressure.


#Plotting a graph to show the correlation between Western Diet score and Systolic blood pressure
plot(clean_data$Western_diet_score,clean_data$Systolic_blood_pressure,
     xlab = "Western Diet Score", ylab = "Systolic Blood Pressure",
     main = "Scatterplot to show the correlation between
     western diet score and Systolic Blood Pressure")
#There is no correlation between systolic blood pressure and Western Diet score.

#Plotting a graph to show the correlation between Systolic blood pressure and GDP/USD
plot(clean_data$GDP_USD,clean_data$Systolic_blood_pressure,
     xlab = "GDP/USD", ylab = " Systolic Blood Pressure",
     main = "Scatterplot to show the correlation
     between GDP/USD and Systolic blood pressure")
#There is no correlation between the GDP in USD and systolic blood pressure.







---------------------------------------------------------------------------------------------------------------------------------------------------

#Plot a bar graph showing the total observations within each Superregion
ggplot(data = clean_data) + theme(legend.position = "none") +
  geom_bar(mapping = aes(x = Superregion, fill = Superregion)) +
  coord_flip() +
  labs(title = "Population Sample in Superregion", x = "Superregion", y = "Population Sample")
##Sub-Saharan Africa had the highest observations of the Superregions by more than 1000 with High-income Asia Pacific having by far the fewest number of observations. Other than South Asia, the remaining Superregions had between 1000-2500 observations.

#Plot a frequency polygon graph showing the mean BMI of children by their sex.
ggplot(data = clean_data, mapping = aes(x = Mean_BMI_children, colour = Sex)) +
  geom_freqpoly(binwidth = 0.1)
##More observations of boys with a BMI on the lower end of the scale than girls, however the peak of observations of girls' BMI is slightly lower than the peak of the boys' BMI.

#Plot an identical frequency polygon graph for mean BMI of adults by sex.
ggplot(data = clean_data, mapping = aes(x = Mean_BMI_children, colour = Sex)) +
  geom_freqpoly(binwidth = 0.1) +
  labs(title = "Freqpoly of Mean BMI in Children by Sex", x = "Mean BMI in Children", y = "Count", colour = "Sex")
##Frequency count peaks a similar points for both male and female BMI, around 25. Higher frequency count for males at the lower end of the BMI scale and higher frequency count for females at the higher end of the BMI scale.


#Plot a frequency polygon graph showing mean BMI of children by Superregion.
ggplot(data = clean_data, mapping = aes(x = Mean_BMI_adults, colour = Sex)) +
  geom_freqpoly(binwidth = 0.1) +
  labs(title = "Freqpoly of Mean BMI in Adults by Sex", x = "Mean BMI in Adults", y = "Count", colour = "Sex")
##Most observations in Sub-Saharan Africa, followed by High-income Western Countries.
##Oceania shows the most observations at the higher end of the BMI scale, with South Asia's observations peaking at the lower end of the BMI scale but with fewer observations overall.


#Plot a histogram showing the overall number of observations at each bin of the BMI scale and each bin being divided by Superregion - different visualisation of previous freqpoly.
ggplot(clean_data) +
  geom_histogram(mapping = aes(x = Mean_BMI_children, fill = Superregion), binwidth = 0.5) +
  labs(title = "Bargraph of Mean BMI in Children by Superregion", x = "Mean BMI in Children", y = "Count", colour = "Superregion")
##High income western countries more prominent in the higher end of the BMI scale with sub-saharan africa being more prominent at the lower end of the BMI scale. Oceania showing most observations at high end of the BMI scale.



#Plot a frequency polygon graph showing BMI of adults by Superregion.
ggplot(data = clean_data, mapping = aes(x = Mean_BMI_adults, colour = Superregion)) +
  geom_freqpoly(binwidth = 0.1) +
  labs(title = "Frequency Polygon Graph of Mean BMI in Adults by Superegion", x = "Mean BMI in Adults", y = "Count", colour = "Superregion")
##Similar to the results seen in the children's data, sub-saharan africa observations are most prominent at the lower end of the BMI scale, high income western countries sitting slighting higher, with oceania showing most of its observations at the higher end of the scale.


#Plot a histogram showing number of observations at each bin of the BMI scale, each bin divided by Superregion - again, a different visualisation of previous freqpoly.
ggplot(clean_data) +
  geom_histogram(mapping = aes(x = Mean_BMI_adults, fill = Superregion), binwidth = 0.5) +  
  labs(title = "Bargraph of Mean BMI in Adults by Superregion", x = "Mean BMI Adults", y = "Count", colour = "Superregion")
##Further confirms results shown in freqpoly previously plotted of the same data.


#Plot a boxplot of mean BMI of children divided by sex to show the medians and outliers more clearly.
ggplot(data = clean_data, mapping = aes(x = Sex, y = Mean_BMI_children)) +
  geom_boxplot() +
  labs(title = "Mean BMI Children by Sex", x = "Sex", y = "Mean BMI Children")
##Girls see a slightly higher median than that of boys. 
##Both see outliers but no value that would be deemed impossible for a child's BMI.


#Plot a histogram of the same data just presented in the boxplot (mean BMI of children by sex) - shows value of outliers more clearly.
ggplot(clean_data) +
  geom_histogram(mapping = aes(x = Mean_BMI_children), binwidth = 0.1) +
  coord_cartesian(ylim = c(0,50)) +
  labs(title = "Mean BMI Children", x = "Mean BMI Children", y = "Count")
##Despite outliers for both male and female children's BMI around 25, this is plausible as children's BMI at 11 years old typically lies within the 14.08 and 25.91 range.


#Plot a boxplot of mean BMI of adults divided by sex to, again, show medians and outliers more clearly.
ggplot(data = clean_data, mapping = aes(x = Sex, y = Mean_BMI_adults)) +
  geom_boxplot() + 
  labs(title = "Mean BMI adults by Sex", x = "Sex", y = "Mean BMI Adults")
##Mean Female BMI median sits slightly higher than that of mean male BMI.
##Mean female BMI sees outliers on both the lower and higher end of the BMI scale with mean male BMI seeing outliters on the higher end of the scale.


#Plot a histogram of the same data just presented in the boxplot (Mean BMI adults by sex) - shows values of outliers more clearly.
ggplot(clean_data) +
  geom_histogram(mapping = aes(x = Mean_BMI_adults), binwidth = 0.1) +
  coord_cartesian(ylim = c(0,50)) +
  labs(title = "Mean BMI in Adults", x = "Mean BMI Adults", y = "Count")
##The apparent outliers for adult's BMI are also not impossible as the ideal range falls between 18.5-24.9
##25-29.9 is classed as the 'overweight' category with 30 and above being the 'obese' category but still plausible results.


#Plot a frequency polygon graph of diabetes prevalence in males and females.
ggplot(data = clean_data, mapping = aes(x = Diabetes_prevalence, colour= Sex)) +
  geom_freqpoly(binwidth = 0.01) +
  labs(title = "Diabetes Prevalence in Sex", x = "Diabetes Prevalence", y = "Count", colour = "Sex")
##Diabetes prevalence in males and females remain at very similar levels throughout the scale, slightly more females with a prevelance between 0.1-0.15


#Plot a frequency polygon of diabetes prevalence by Superregion
ggplot(data = clean_data, mapping = aes(x = Diabetes_prevalence, colour = Superregion)) +
  geom_freqpoly(binwidth = 0.01) +
  labs(title = "Diabetes Prevalence per Superregion", x = "Superregion", y = "Count", colour = "Diabetes Prevelance")
##Similar to the BMI data, Sub-Saharan Africa sees most of its observations on the lower end of the scale. Oceania has a more continuous line when compared to BMI but still remains higher than all other Superregions towards the higher end of the scale on the x axis.


#Plot a boxplot of diabetes prevalence by Superregion ordered by medians to clearly see medians and outliers.
ggplot(data = clean_data) +
  geom_boxplot(
    mapping = aes(
      x = reorder(Superregion, Diabetes_prevalence, FUN = median),
      y = Diabetes_prevalence
    )) + 
  coord_flip() +
  labs(title = "Diabetes Prevalence per Superregion", x = "Superregion", y = "Diabetes Prevalence", colour = "Diabetes_prevelance")
##Oceania with a clear highest median of diabetes prevalence, even compared to second placed Central Asia and North Africa-Middle East.
##The gap between medians reduces from there.


#Count values within the variables of sex and superregion
clean_data %>%
  count(Superregion, Sex)

#Plot a heatmap of the interaction between mean BMI of children and diabetes prevalence 
ggplot(data = clean_data) +
  geom_hex(mapping = aes(x = Mean_BMI_children, y = Diabetes_prevalence))  +
  labs(title = "Heatmap of Mean BMI in Children and Diabetes Prevalence", x = "Mean BMI in Children", y = "Diabetes Prevalence", colour = "Count")
##General trend as mean BMI in children increases, diabetes prevalence also increases.
##There are some outliers in that trend with instances of high diabetes prevalence and BMI that is not particularly high, and high frequency counts of these cases too.


#Plot a heatmap of the interaction between mean BMI of adults and diabetets prevalence
ggplot(clean_data) +
  geom_hex(mapping = aes(x = Mean_BMI_adults, y = Diabetes_prevalence)) +
  labs(title = "Heatmap of Mean BMI in Adults and Diabetes Prevalence", x = "Mean BMI in Adults", y = "Diabetes Prevalence", colour = "Count")
##A clearer trend can be seen between mean BMI of adults and diabetes prevalence with far fewer outliers to that trend.


#Plot a boxplot of the interaction between mean BMI in children and diabetes prevalence in order to see median and outliers clearly.
ggplot(data = clean_data, mapping = aes(x = Mean_BMI_children, y = Diabetes_prevalence)) +
  geom_boxplot(mapping = aes(group = cut_width(Mean_BMI_children, 0.5))) +
  labs(title = "Boxplot of Mean BMI in Children and Diabetes Prevalence", x = "Mean BMI in Children", y = "Diabetes Prevalence")
##The outliers seen when plotting mean BMI in children and diabetes prevalence become clearer in a boxplot. The trend between the two variables is also more apparent in this visualisation.


#Plot a boxplot of the interaction between mean BMI in adults and diabetes prevalence in order to see medians and outliers clearly
ggplot(data = clean_data, mapping = aes(x = Mean_BMI_adults, y = Diabetes_prevalence)) +
  geom_boxplot(mapping = aes(group = cut_width(Mean_BMI_adults, 0.5))) +
  labs(title = "Boxplot of Mean BMI in Adults and Diabetes Prevalence", x = "Mean BMI in Adults", y = "Diabetes Prevalence")
##Clear trend remains visible when plotting mean BMI adults and diabetes prevalence but the boxplot does expose outliers more clearly - more present at mid-low BMI scale potentially due to type 1/2 diabetes.

  
---------------------------------------------------------------------------------------------------------------------------------------------------

#Looking at the first few rows of the data
head(data)

  #Creating sample
set.seed(380)
n <- nrow(data)
index_selected <- sample(1:n, size = 3000, replace = TRUE)
Global_sample1 <- data[index_selected, ]
#Getting rid of N/A's
Global_sample <- na.omit(Global_sample1)
View(Global_sample)
#Counting number of N/A's in each column
sapply(Global_sample, function(x) sum(is.na(x)))

## Weight and Obesity in Adults

#Looking at obesity in sample
ggplot(data = Global_sample)+ geom_histogram(mapping = aes(x = Prevalence_obesity_adults))+ labs(x="Prevalence of Obesity in Adults", y="Frequency", title = "Histogram of the Prevalence of Obesity in Adults")

#Looking at this histograms shows that low prevalence of obesity is most common in the sample.

#Looking at correlation between underweight adults and sex
ggplot(data = Global_sample, mapping = aes(x = Prevalence_underweight_adults, colour = Sex)) +
  geom_freqpoly(binwidth = 0.01)+labs(x="Prevalence of Underweight Adults", y="Frequency", title="Graph showing the Prevalence of Underweight Adults")

#We can see from this graph that more males from around the world are underweight than females.

#Looking at correlation between underweight adults and superrregion
ggplot(data = Global_sample, mapping = aes(x = Prevalence_underweight_adults, colour = Superregion)) +
  geom_freqpoly(binwidth = 0.01)+labs(x="Prevalence of Underweight Adults", y="Frequency", title="Prevalence of Underweight Adults by Superregion")

#This graph shows a high prevalence of underweight adults in sub-saharan Africa and South Asia. Central and Eastern Europe, high-income Western countries, and high-income Asian Pacific countries have lower prevalence of underweight adults.
#Correlation between morbidly obese adults and superregion
ggplot(data = Global_sample, mapping = aes(x = Prevalence_morbid_obesity_adults, colour = Superregion)) +
  geom_freqpoly(binwidth = 0.01)+labs(x="Prevalence of Morbidly Obese Adults",title = "Prevelence of Morbidly Obese Adults in Differing Superregions", y="Frequency")

#This graph shows a high prevalence of morbidly obese adults in Oceania and low prevalence in Sub-Saharan Africa.

## Results and Conclusions

--------------------------------------------------------------------------------------------------------------------------------------------------
  # select variables for clustering
  # the data with numerical values has been selected to do the clustering to avoid erorrs
  data_subset <- data[, c("Mean_BMI_children", "Prevalence_obesity_children", "Prevalence_overweight_children",
                          "Prevalence_underweight_children", "Mean_BMI_adults", "Prevalence_obesity_adults",
                          "Prevalence_underweight_adults", "Prevalence_morbid_obesity_adults",
                          "Diabetes_prevalence", "Systolic_blood_pressure", "Prevalence_raised_blood_pressure",
                          "Years_of_education", "Urbanisation", "Western_diet_score", "GDP_USD")]

# remove rows with missing or infinite values
# this removes any rows with missing values or na values
scaled_data <- na.omit(data_subset)

# determine the optimal number of clusters using the elbow method
set.seed(456) 
# sets the seed for the random number generator to ensure reproducibility of the results.
wss <- (nrow(scaled_data) - 1) * sum(apply(scaled_data, 2, var)) 
# calculates the within-cluster sum of squares for each number of clusters (k) from 1 to 15. The formula used here is the product of the number of observations minus 1 and the sum of variances of all variables.
for (i in 2:15) wss[i] <- sum(kmeans(scaled_data, centers = i)$withinss)
# performs k-means clustering for each k from 2 to 15 and calculates the sum of within-cluster sum of squares for each k.
ggplot(data.frame(k = 1:15, WSS = wss[1:15]), aes(x = k, y = WSS)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of clusters",
       y = "Within-cluster sum of squares")
# The WSS (Within-Cluster Sum of Squares) measures the sum of the squared distances between each point and its assigned centroid in each cluster. The k value represents the number of clusters being evaluated.
# The elbow point on the graph is the point where the rate of decrease in WSS begins to level off, indicating that additional clusters beyond that point may not provide much more explanatory power. This point is used to determine the optimal number of clusters for the data.

# perform clustering with the optimal number of clusters
# optimal clusters from the graph seems to be 4 so we will use that
num_clusters <- 4
# Performs K-means clustering on the scaled data scaled_data with num_clusters number of clusters and assigns the resulting model to the variable cluster_model.
cluster_model <- kmeans(scaled_data, centers = num_clusters)

# add the cluster labels to the original data
# Adds a new column to scaled_data called "cluster" and assigns the cluster labels generated by the K-means clustering model stored in cluster_model to this new column.
scaled_data$cluster <- cluster_model$cluster

# calculate the mean of each variable within each cluster and stores the resulting data 
# The aggregate function groups the data in scaled_data by the cluster labels stored in cluster_model$cluster, and the mean function calculates the mean of each variable within each cluster.
cluster_means <- aggregate(scaled_data, by = list(cluster_model$cluster), mean)

# print the cluster means
# Prints the resulting data frame cluster_means containing the mean of each variable within each cluster to the console.
print(cluster_means)

# scatter plot of usd and years of education
# This code creates a scatter plot of GDP_USD and Years_of_education, with the points colored by their assigned cluster. The plot is labeled with a title and axes labels.
ggplot(data = scaled_data, aes(x = GDP_USD, y = Years_of_education, color = factor(cluster))) +
  geom_point(size = 1) +
  scale_color_discrete(name = "Cluster") +
  labs(title = "Cluster differences in GDP_USD and Years of education",
       x = "GDP_USD",
       y = "Years of education")

---------------------------------------------------------------------------------------------------------------------------------------------------

#Looking at correlation between underweight adults and super region
ggplot(data = scaled_data, mapping = aes(x = Prevalence_underweight_adults, colour = factor(cluster))) +scale_color_discrete(name = "Cluster")+
  geom_freqpoly(binwidth = 0.005)+labs(x="Prevalence of Underweight Adults", y="Frequency", title="Prevalence of Underweight Adults by Cluster")

#This graph shows that cluster 2 has the lowest prevalence of underweight adults. Cluster 4 has the highest prevalence of underweight adults.

ggplot(data = scaled_data, mapping = aes(x = Prevalence_obesity_adults, colour = factor(cluster))) +scale_color_discrete(name = "Cluster")+
  geom_freqpoly(binwidth = 0.01)+labs(x="Prevalence of Obesity in Adults", y="Frequency", title="Prevalence of Obesity in Adults by Cluster")

#Cluster 4 has the highest frequency for the lowest of obesity in adults. Cluster 2 and 1 have higher prevalences around the 0.2 mark.

ggplot(data = scaled_data, mapping = aes(x = Prevalence_morbid_obesity_adults, colour = factor(cluster))) +scale_color_discrete(name = "Cluster")+
  geom_freqpoly(binwidth = 0.005)+labs(x="Prevalence of Morbid Obesity in Adults", y="Frequency", title="Prevalence of Morbid Obesity in by Cluster")

#Cluster 4 has the highest frequency of low prevalence of morbid obesity in adults, whereas cluster 3 has the lowest frequency of low prevalence. 

#Comparing obesity and weight
scaled_data$cluster<-as.character(scaled_data$cluster)
ggpairs(scaled_data,                 # Data frame
        columns = 6:8,        # Columns
        aes(color = scaled_data$cluster,  # Color by group 
            alpha = 0.5))

---------------------------------------------------------------------------------------------------------------------------------------------------
  
  # Classification
  
  # split the data into training and test sets
  set.seed(456)
# sets a seed to ensure reproducibility
train_index <- createDataPartition(data$Prevalence_obesity_children, p = 0.7, list = FALSE)
# createDataPartition() function from the caret package is used to randomly split the data into two sets based on a specified proportion. In this case, the target variable Prevalence_obesity_children is used to split the data into a 70% training set and a 30% test set. The argument list = FALSE is used to return a vector of row indices instead of a list of row index vectors.
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
# The row indices obtained from createDataPartition() are used to subset the original data into a training set (train_data) and a test set (test_data) using the bracket notation.

# Fit the random forest model
# Find missing values
missing_vals <- apply(train_data, 2, function(x) any(is.na(x)))
train_data <- train_data[, !missing_vals]
# The missing_vals variable finds any columns in train_data with missing values using the apply() function and a custom function that checks for NA values. train_data is then subsetted to remove the columns with missing values.

# Build the model
rf_model <- randomForest(Prevalence_obesity_children ~ ., data = train_data, importance = TRUE)
# randomForest() function from the randomForest package is used to build a random forest model with Prevalence_obesity_children as the dependent variable and all other variables as independent variables. The importance parameter is set to TRUE to calculate variable importance measures. The resulting model is stored in the rf_model variable.

# tune the random forest model
set.seed(456)
# sets a seed for reproducibility purposes.
tune_grid <- expand.grid(.mtry = c(4, 6, 8, 10),
                         .ntree = c(500, 1000, 2000))

# .mtry refers to the number of variables randomly sampled as candidates at each split. c(4, 6, 8, 10) specifies the values to be tested for mtry.
# .ntree refers to the number of trees grown in the forest. c(500, 1000, 2000) specifies the values to be tested for ntree.

# This code may take awhile to run because of the amount of data
rf_tune <- tuneRF(train_data[, -c(1, 2, 3)], train_data$Prevalence_obesity_children,
                  ntreeTry = 500,
                  mtryTry = tune_grid$mtry,
                  stepFactor = 1.5,
                  improve = 0.01,
                  trace = TRUE,
                  plot = TRUE)

# rf_tune tunes the random forest model with the tuneRF() function, which tests various combinations of mtry (number of variables randomly sampled at each split) and ntree (number of trees in the forest).
# train_data[, -c(1, 2, 3)] selects all the columns in train_data except the first three (assuming they are not used as predictors).
# train_data$Prevalence_obesity_children is the target variable that we want to predict.
# ntreeTry specifies the number of trees to grow.
# mtryTry specifies the different values of mtry to test.
# stepFactor specifies the multiplicative factor used to increase the number of variables selected for each split.
# improve is the minimum improvement in the error rate needed to make a split.
# trace displays progress and results of the tuning process.
# plot generates a plot of the error rate versus the number of trees.

# What rf_tune shows?
# The function then shows the values of the OOB error rate for different mtry values to the left and right of the optimal value. The stepFactor argument is used to determine the step size between mtry values in the search for the optimal value. In this case, the step factor is set to 1.5.
# The graph shows the trend of the OOB error rate as the mtry value changes. The graph is used to identify the optimal mtry value. The graph shows a downward trend in the OOB error rate as the mtry value increases up to a certain point, and then the OOB error rate starts to increase again. The optimal mtry value is the point at which the OOB error rate is minimized. In this case, the optimal mtry value is 9, as shown in the output.


# Plot feature importance
varImpPlot(rf_model, main = "Feature Importance Plot")
# This line creates a plot showing the importance of each feature in the random forest model.

# Get feature importance values
importance_values <- importance(rf_model)
# This line stores the importance values of each feature in the random forest model into the variable importance_values.
print(importance_values)
# This line prints the importance values of each feature in the random forest model.

# Sort feature importance values in decreasing order
sorted_importance <- sort(importance_values[,"%IncMSE"], decreasing = TRUE)
# This line sorts the importance values of each feature in decreasing order based on the %IncMSE metric, and stores the sorted values into the variable sorted_importance.
print(sorted_importance)
# This line prints the sorted feature importance values in decreasing order.

# What does sorted importance mean?
# This output shows the feature importance values sorted in decreasing order. The first row shows the feature with the highest importance, while the last row shows the feature with the lowest importance. The values in the rows indicate the percentage increase in mean squared error (%IncMSE) when the feature is randomly permuted, which is a measure of how important the feature is for prediction.

# Plot sorted feature importance values
barplot(sorted_importance, main = "Sorted Feature Importance Plot")
# This plot shows a simple view of the importance of each factor

# Create a dataframe with the variable names and importance values
df <- data.frame(variable = names(sorted_importance), importance = sorted_importance)
# This line creates a data frame called df with two columns: variable and importance.
# The variable column contains the names of the variables (features) in the dataset that were used to build the random forest model.
# The importance column contains the corresponding importance values for each variable, which were previously sorted in descending order using the sort function.

# Reorder the dataframe by importance values in descending order
df <- df[order(df$importance, decreasing = TRUE),]

# Create a ggplot barplot
ggplot(df, aes(x = reorder(variable, importance), y = importance, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Sorted Feature Importance Plot") +
  xlab("Variable") +
  ylab("Importance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_viridis_d(option = "H")
# This plot shows the most important factors to consider when looking at prevalence in child obesity

---------------------------------------------------------------------------------------------------------------------------------------------------
