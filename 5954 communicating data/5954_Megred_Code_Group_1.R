# Load Packages
##### if any packages are not installed type #####################################
#### install.packages("NAME OF PACKAGE") #########################################
#### in console ##################################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)
library(randomForest)
library(viridisLite)
library(ggridges)
library(forcats)
library(tidyverse)
library(hexbin)
library(hnp)
library(ggupset)
library(htmltools)
library(hrbrthemes)
library(viridis)
library(ggExtra)
library(cowplot)
library(colorspace)
library(GGally)
library(countrycode)
library(gapminder)
library(tibble)
library(lemon)
library(patchwork)
library(lubridate)
library(plotly)
library(ggrepel)

####################################################################################
#### READ ME #######################################################################
#### Please run each section at a time then clear objects from work space before ###
#### running a different section as there may be overlapping varibles as this file #
#### was an attempt to merge 4 peoples code into a single file and so there may be #
#### errors if you run mulitple sections without clearing the workspace ############
####################################################################################


#######################################################################################
#######################################################################################
#######################################################################################

#Section 1

# read in the data
# this reads in the data into a table so we are able to use it within the code
data <- read.csv("LifeExpectancyData.csv")

# remove rows with missing or infinite values
# this removes any rows with missing values or na values
clean_data <- na.omit(data)

# Classification for life expectency

# split the data into training and test sets
set.seed(456)
# sets a seed to ensure reproducibility
train_index <- createDataPartition(clean_data$Life.expectancy, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Fit the random forest model
# Find missing values
missing_vals <- apply(train_data, 2, function(x) any(is.na(x)))
train_data <- train_data[, !missing_vals]

# Build the model
rf_model <- randomForest(Life.expectancy ~ ., data = train_data, importance = TRUE)

# tune the random forest model
set.seed(456)
# sets a seed for reproducibility purposes.
tune_grid <- expand.grid(.mtry = c(4, 6, 8, 10),
                         .ntree = c(500, 1000, 2000))

# This code may take awhile to run because of the amount of data
rf_tune <- tuneRF(train_data[, -c(1, 2, 3)], train_data$Life.expectancy,
                  ntreeTry = 500,
                  mtryTry = tune_grid$mtry,
                  stepFactor = 1.5,
                  improve = 0.01,
                  trace = TRUE,
                  plot = TRUE)

# Plot feature importance
varImpPlot(rf_model, main = "Feature Importance Plot")

# Get feature importance values
importance_values <- importance(rf_model)
print(importance_values)

# Sort feature importance values in decreasing order
sorted_importance <- sort(importance_values[,"%IncMSE"], decreasing = TRUE)
print(sorted_importance)

# Plot sorted feature importance values
barplot(sorted_importance, main = "Sorted Feature Importance Plot")

# Create a dataframe with the variable names and importance values
df <- data.frame(variable = names(sorted_importance), importance = sorted_importance)

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

# Improved Plot

ggplot(df, aes(x = importance, y = reorder(variable, importance))) +
  geom_col(fill = "skyblue") +
  labs(title = "Feature Importance for Predicting Life expectancy",
       subtitle = "Top Variables Ranked by Importance",
       x = "Importance",
       y = "Varible") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  coord_flip()

# Final Plot

ggplot(df, aes(x = importance, y = reorder(variable, importance))) +
  geom_col(fill = "skyblue") +
  labs(title = "Feature Importance for Predicting Life Expectancy",
       subtitle = "Top Variables Ranked by Importance",
       x = "Importance",
       y = "") +
  scale_y_discrete(labels = c("Status",
                              "Measles per 1000 population",
                              "Polio immunization coverage",
                              "Diphtheria coverage",
                              "Expenditure on health",
                              "Infant Deaths per 1000 population",
                              "Year",
                              "Under-five deaths per 1000 population",
                              "Average Body Mass Index",
                              "Thinness among children Age 5 to 9",
                              "Country",
                              "Thinness among children Age 10 to 19",
                              "Adult Mortality Rates per 1000 population",
                              "HIV/AIDS")) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  coord_flip()

#######################################################################################
#######################################################################################
#######################################################################################

#Section 2 

##5954 Group Presentation

Life.Expectancy.Data1 <- read.csv("~/Data Analytics/5954 data/Life Expectancy Data.csv")
View(Life.Expectancy.Data)

Life.Expectancy.Data<-na.omit(Life.Expectancy.Data1)


## Ridge Graphs
#Life.expectancy
ggplot(Life.Expectancy.Data, aes(x = Life.expectancy, y =Status, fill = Status )) +
  geom_density_ridges() +
  labs(title = "Life Expectancy in Developing/Developed Countries", x="Life Expectancy (Age)",
       caption = "Life expectancy is higher in developed countries." )+
  theme_ridges() + 
  theme(legend.position = "none")

#Hepatitis.B/life expectancy
ggplot(Life.Expectancy.Data, aes(x = Hepatitis.B, y =Status, fill = Status )) +
  geom_density_ridges() +
  theme_ridges() + 
  labs(title = "Hepatitis B in Developed/Developing Countries", subtitle ="HepB immunization coverage in 1 year olds", caption="Higher coverage in developed countries", x="Hepatitis B (%)" )+
  theme(legend.position = "none")

## Boxplots
#hepatitis/status
Life.Expectancy.Data %>%
  ggplot( aes(x=Status, y=Hepatitis.B, fill=Status)) +
  geom_boxplot() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  labs(title = "Hepatitis B in Developed/Developing Countries",
       subtitle ="HepB immunization coverage in 1 year olds", 
       caption="Higher coverage in developed countries", y="Hepatitis B (%)" )

## Bubble plot
#hepatitis/GDP/Life expectancy
ggplot(Life.Expectancy.Data, aes(x=Hepatitis.B, y=Life.expectancy, size = GDP, color=Status)) +
  geom_point(alpha=0.7)+
  labs(title = "Hepatitis B immunisation and Life Expectancy", subtitle ="HepB immunization coverage in 1 year olds", caption="Higher coverage in countries which are developed and have a high GDP.", x="Hepatitis B (%)", y="Life Expectancy by Age" )+
  theme_light()


#Explanatory
head(Life.Expectancy.Data)

#Making test and train data sets
set.seed(38)
trainIndex <- createDataPartition(Life.Expectancy.Data$Life.expectancy,
                                  p = 2/3,
                                  list = FALSE,
                                  times = 1)
life_train <- Life.Expectancy.Data[ trainIndex, ]
life_test <- Life.Expectancy.Data[-trainIndex, ]

#Variable selection
glm_mod2 <- train(
  form = Life.expectancy ~ Year+Status+Adult.Mortality+infant.deaths+percentage.expenditure+Hepatitis.B+Measles+BMI+under.five.deaths+Polio+Total.expenditure+Diphtheria+Alcohol+HIV.AIDS+GDP+Population+thinness..1.19.years+thinness.5.9.years+Income.composition.of.resources+Schooling,
  data = life_train,
  method = "glmStepAIC",
  family = "gaussian")

summary(glm_mod2$finalModel)

#Call:#########################################################################
#  NULL
#
#Deviance Residuals: 
# Min        1Q    Median        3Q       Max  
#-16.6098   -2.2046    0.0131    2.1873   12.1236  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                      3.260e+02  5.646e+01   5.774 1.01e-08 ***
#  Year                            -1.357e-01  2.822e-02  -4.810 1.72e-06 ***
#  StatusDeveloping                -1.258e+00  4.113e-01  -3.059  0.00227 ** 
#  Adult.Mortality                 -1.545e-02  1.123e-03 -13.760  < 2e-16 ***
#  infant.deaths                    8.809e-02  1.362e-02   6.470 1.48e-10 ***
#  percentage.expenditure           5.085e-04  8.251e-05   6.162 1.01e-09 ***
#  Hepatitis.B                     -1.044e-02  5.635e-03  -1.852  0.06427 .  
#BMI                              3.171e-02  7.233e-03   4.384 1.28e-05 ***
#  under.five.deaths               -6.732e-02  1.026e-02  -6.564 8.09e-11 ***
#  Polio                            1.193e-02  6.466e-03   1.845  0.06537 .  
#Total.expenditure                7.273e-02  4.894e-02   1.486  0.13751    
#Diphtheria                       2.267e-02  7.284e-03   3.113  0.00190 ** 
#  Alcohol                         -1.656e-01  4.100e-02  -4.039 5.75e-05 ***
#  HIV.AIDS                        -4.341e-01  1.936e-02 -22.428  < 2e-16 ***
#  thinness..1.19.years            -4.493e-02  3.163e-02  -1.420  0.15578    
#Income.composition.of.resources  1.081e+01  1.041e+00  10.381  < 2e-16 ***
#  Schooling                        8.694e-01  7.397e-02  11.754  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 12.55395)
#
#Null deviance: 87194  on 1100  degrees of freedom
#Residual deviance: 13608  on 1084  degrees of freedom
#AIC: 5928.9
#
#Number of Fisher Scoring iterations: 2
################################################################################

#Saving results
saveRDS(glm_mod2,
        "glm_mod.rds")
glm_mod2 <- readRDS("glm_mod.rds")

summary(glm_mod2$finalModel)
glm_mod2$finalModel

#looking at residuals
hnp(glm_mod2$finalModel)

#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

###FAILED MODELS THAT PROVE GLM_MOD2 IS THE BEST FIT

glm_mod3 <- train(
  form = Life.expectancy ~ Year+Status+Adult.Mortality+infant.deaths+percentage.expenditure+Hepatitis.B+Measles+BMI+under.five.deaths+Polio+Total.expenditure+Diphtheria+Alcohol+HIV.AIDS+GDP+Population+thinness..1.19.years+thinness.5.9.years+Income.composition.of.resources+Schooling,
  data = life_train,
  method = "glmStepAIC",
  family = "inverse.gaussian")
summary(glm_mod3$finalModel)
hnp(glm_mod3$finalModel)

glm_mod4 <- train(
  form = Life.expectancy ~ Year+Status+Adult.Mortality+infant.deaths+percentage.expenditure+Hepatitis.B+Measles+BMI+under.five.deaths+Polio+Total.expenditure+Diphtheria+Alcohol+HIV.AIDS+GDP+Population+thinness..1.19.years+thinness.5.9.years+Income.composition.of.resources+Schooling,
  data = life_train,
  method = "glmStepAIC",
  family = "Gamma")
summary(glm_mod4$finalModel)
hnp(glm_mod4$finalModel)

glm_mod5 <- train(
  form = Life.expectancy ~ Year+Status+Adult.Mortality+infant.deaths+percentage.expenditure+Hepatitis.B+Measles+BMI+under.five.deaths+Polio+Total.expenditure+Diphtheria+Alcohol+HIV.AIDS+GDP+Population+thinness..1.19.years+thinness.5.9.years+Income.composition.of.resources+Schooling,
  data = life_train,
  method = "glmStepAIC",
  family = "Poisson")
summary(glm_mod5$finalModel)
hnp(glm_mod5$finalModel)

attach(Life.Expectancy.Data)
glm_mod6 <- train(
  form = Life.expectancy ~ Year+Status+Adult.Mortality+infant.deaths
  +percentage.expenditure+Hepatitis.B+Measles+BMI+under.five.deaths
  +Polio+Total.expenditure+Diphtheria+Alcohol+HIV.AIDS+GDP
  +Population+thinness..1.19.years+thinness.5.9.years
  +Income.composition.of.resources+Schooling
  +Total.expenditure*Schooling+under.five.deaths*Measles+under.five.deaths*Polio+Total.expenditure*GDP,
  data = life_train,
  method = "glmStepAIC",
  family = "gaussian")
summary(glm_mod6$finalModel)
hnp(glm_mod6$finalModel)

#looking at correlations between variables
numeric<-select_if(Life.Expectancy.Data, is.numeric)
cor(numeric)

#correlation graph for all variables
library(corrplot)
corrplot(cor(numeric), #correlation matrix
         type = 'lower', #displaying one side of matrix
         order = 'hclust', #ordering method
         tl.col = 'black', #label colour
         cl.ratio = 0.1, #legend size
         tl.srt = 360, #label angle
         method = 'square', #shape of symbol
         addCoef.col = 'black', #colour of correlation coeficients
         number.cex = 0.6, #size of correlation coeficients
         tl.cex = 0.5, #size of labels
         col = COL2('RdYlBu'))# colour scheme


numeric2<-data.frame(Life.expectancy,Adult.Mortality,HIV.AIDS, thinness..1.19.years,thinness.5.9.years,BMI, Income.composition.of.resources,Schooling,Alcohol,percentage.expenditure,GDP)
names(numeric2)<-c("Life Expectancy","Adult Mortality","HIV/AIDS", "Thinness 10-19 Years","Thinness 5-9 Years","BMI", "Income Composition of Resources","Schooling","Alcohol","Percentage Expenditure","GDP")
corrplot(cor(numeric2), #correlation matrix
         type = 'lower', #displaying one side of matrix
         order = 'hclust', #ordering method
         tl.col = 'black', #label colour
         cl.ratio = 0.1, #legend size
         tl.srt = 360, #label angle
         method = 'square', #shape of symbol
         addCoef.col = 'black', #colour of correlation coeficients
         number.cex = 0.6, #size of correlation coeficients
         tl.cex = 0.7, #size of labels
         col = COL2('RdYlBu'))# colour scheme


glm_mod7 <- train(
  form = Life.expectancy ~ Year+Status+Adult.Mortality+infant.deaths
  +percentage.expenditure+Hepatitis.B+Measles+BMI+under.five.deaths
  +Polio+Total.expenditure+Diphtheria+Alcohol+HIV.AIDS+GDP
  +Population+thinness..1.19.years+thinness.5.9.years
  +Income.composition.of.resources+Schooling
  +Total.expenditure*Schooling+Total.expenditure*GDP
  +Life.expectancy*Adult.Mortality+HIV.AIDS*Life.expectancy+thinness..1.19.years*BMI+BMI*thinness.5.9.years
  +Income.composition.of.resources*Schooling+Life.expectancy*Income.composition.of.resources+Schooling*Life.expectancy
  +infant.deaths*Population+thinness..1.19.years*thinness.5.9.years+infant.deaths*under.five.deaths,
  data = life_train,
  method = "glmStepAIC",
  family = "gaussian")
summary(glm_mod7$finalModel)
hnp(glm_mod7$finalModel)
#/////////////////////////////////////////////////////////////////////////////////////////////

#variable importance
poplife<-caret::varImp(glm_mod2)
poplife

#loess r-squared variable importance

#Overall
#Income.composition.of.resources 100.000
#HIV.AIDS                         89.828
#Adult.Mortality                  84.953
#Schooling                        82.229
#thinness.5.9.years               80.377
#thinness..1.19.years             79.581
#under.five.deaths                63.376
#BMI                              59.111
#infant.deaths                    57.023
#Polio                            43.930
#percentage.expenditure           43.826
#GDP                              43.066
#Diphtheria                       42.991
#Hepatitis.B                      35.502
#Status                           27.317
#Alcohol                          23.481
#Total.expenditure                 9.401
#Measles                           5.938
#Year                              1.651
#Population                        0.000

#renaming row column
somenames<-data.frame(names(Life.Expectancy.Data))
somenames2<-data.frame(somenames[-c(1),])

poplife3<-data.frame(poplife$importance)
somenames2
poplife2
row.names(poplife3)
poplifegood<-data.frame(poplife3, row.names(poplife3))
poplifegood

# Importance Bargraph
p<-ggplot(data=poplifegood, aes(x=reorder(row.names.poplife3., -Overall), y=Overall)) +
  geom_bar(stat="identity")+
  labs(title = "Variable Importance Graph", x="Variable", y="Rating", subtitle = "Rating of how important each variable is from 0 to 100",
       caption = "Population is the least important and the income composition of resources is the most important.")+theme_light()
# Horizontal bar plot
p + coord_flip()

#######################################################################################
#######################################################################################
#######################################################################################

#Section 3 

life_expectancy_data<-read.csv("Life Expectancy Data.csv")
clean_data<-na.omit(life_expectancy_data)


df<- data.frame(country = c("Afghanistan", "Albania", "Algeria", "Angola", "Antigua and Barbuda", "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei Darussalam", "Bulgaria", "Burkina Faso", "Burundi", "Ivory Coast", "Cabo Verde", "Cambodia", "Cameroon", "Canada", "Central African Republic", "Chad", "Chile", "China", "Colombia", "Comoros", "Congo", "Cook Islands", "Costa Rica", "Croatia", "Cuba", "Cyprus", "Czechia", "Democratic People's Repubic of Korea", "Democratic Republic of the Congo", "Denmark", "Djibouti", "Dominican Republic","Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Ethiopia", "Fiji", "Finland", "France", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Kuwait", "Kyrgyzstan", "Lao People's Democratic Republic", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Lithuania", "Luxembourg", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania", "Mauritius", "Mexico", "Micronesia", "Monaco", "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", "New Zealand", "Nicaragua", "Niger", "Nigeria", "Niue", "Norway", "Oman", "Pakistan", "Palau", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Qatar", "Republic of Korea", "Republic of Moldova", "Romania", "Russia", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", "Swaziland", "Sweden", "Switzerland", "Syrian Arab Republic", "Tajikistan", "Thailand", "Macedonia", "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom of Great Britain and Northern Ireland", "United Republic of Tanzania", "United States of America", "Uruguay", "Uzbekistan", "Vanuatu", "Venezuela", "Viet Nam", "Yemen", "Zambia", "Zimbabwe"))

df$continent<-countrycode(sourcevar = df[,"country"],
                          origin = "country.name",
                          destination = "continent")

ggplot(clean_data) +
  geom_bar(mapping = aes(x = Status), fill = c("firebrick","slateblue")) +
  scale_y_continuous(breaks = seq(0,2000,100)) +
  theme_ipsum() +
  labs(title = "Count of Observations from Developed/Developing Countries in Dataset")


ggplot(clean_data, aes(x = Year, y = infant_deaths, fill = Status)) +
  geom_bar(postion = "stack", stat = "identity") +
  scale_x_continuous(breaks = seq(2000,2015,1)) +
  theme_ipsum() +
  labs(title = "Infant Deaths per Year in Developing and Developed Countries", y = "Infant Deaths")

ggplot(clean_data, aes(x = Status, y = Life_expectancy, fill = Status)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11)
  ) +
  ggtitle("Life Expectancy by Country Status") +
  xlab("") +
  ylab("Life Expectancy")


ggplot(clean_data, aes(x = Life_expectancy, fill = Status)) +
  geom_histogram(color = "#e9ecef", alpha = 0.6, position = 'identity') +
  scale_fill_manual(values = c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill = "Country Status", title = "Life Expectancy in Developed and Developing Countries", caption = "Overlapping Life Expectancy Shown by the Blue Sections in the Bars") +
  xlab("Life Expectancy")

label = "Luxembourg",
x = 112000,
y = 19000,
label.padding = unit(0.55, "lines"),
label.size = 0.2,
color = "black",
fill = "#69b3a2")

ggplot(clean_data, aes(x = GDP, y = percentage_expenditure, label = Country)) +
  geom_point(color = case_when(clean_data$percentage_expenditure > 15000 ~ "#69b3a2",
                               TRUE ~ "black")) +
  geom_smooth(method = lm, color = "red", fill = "#69b3a2", se = TRUE) +
  theme_ipsum() +
  labs(title = "% Expenditure on Heathcare Compared to GDP", caption = "Percentage Expenditure on Heath as % of GDP Compared to Total GDP") +
  ylab("% Expenditure") +
  scale_y_continuous(breaks = seq(0,19000,1000)) +
  geom_label(label = "Luxembourg",
             x = 112000,
             y = 19000,
             label.padding = unit(0.4, "lines"),
             label.size = 0.1,
             color = "black",
             fill = "#69b3a2",
             hjust = "inward")

ggplot(clean_data, aes(x = Schooling, group = Status, fill = Status)) +
  geom_density(adjust = 1.5) +
  theme_ipsum() +
  facet_wrap(~Status) +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x = element_blank()
  ) +
  scale_x_continuous(breaks = seq(0,25,5), limits = c(0,25), sec.axis = dup_axis()) +
  labs(title = "Average Years in Schooling in Developed and Developing Countries")

ggplot(clean_data, aes(x = Alcohol, fill = Status)) +
  geom_histogram (color = "#e9ecef", alpha = 0.6, position = "identity") +
  scale_fill_manual(values = c("#69b3a2", "#404080")) +
  theme(legend.position = "none") +
  theme_ipsum() +
  labs(fill = "", title = "Alcohol Consumption in Developed and Developing Countries", caption = "Overlapping counts of Alcohol Consumptions Shown by Blue Sections of Bars")

ggplot(clean_data, aes(x = Status, y = Adult_Mortality, fill = Status)) +
  geom_boxplot(alpha = 0.3) +
  stat_summary(fun.y = mean, geom = "point", shape = 20, size = 5, colour = "red", fill = "red") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Adult Mortality Rate in Developed and Developing Countries", caption = "Red Dot Shows Mean Adult Mortality Rate")

ggplot(clean_data, aes(fill = Status, y = Life_expectancy, x = Year)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_x_continuous(breaks = seq(2000,2015,1)) +
  scale_y_continuous(breaks = seq(0,90,5)) +
  theme_ipsum() +
  labs(title = "Life Expectancy by Year") +
  xlab("Year") +
  ylab("Life Expectancy")

ggplot(clean_data, aes(x = Status, y = Schooling, fill = Status)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11)
  ) +
  ggtitle("Schooling by Country Status") +
  xlab("") +
  ylab("Schooling")

ggplot(clean_data, aes(x = Life_expectancy, y = Schooling, size = GDP)) +
  geom_point(alpha = 0.7) +
  labs(title = "How Levels of Schooling interacts with Life Expectancty and GDP", x = "Life Expectancy")


model<-lm(Life_expectancy~Year + Status + Adult_Mortality + Alcohol + percentage_expenditure + Hepatitis_B + Measles + BMI + under_five_deaths + Polio + Total_expenditure + HIV_AIDS + GDP + Population + thinness_5_to_9_years + Income_composition_of_resources + Schooling, data = clean_data)

summary(model)

sigmodel<-lm(Life_expectancy~Year + Status + Adult_Mortality + Alcohol + BMI + under_five_deaths + Polio + Total_expenditure + HIV_AIDS + Income_composition_of_resources + Schooling, data = clean_data)

plot(sigmodel)

clean_data$fitted_values<-predict(sigmodel)

ggplot(clean_data) +
  geom_point(aes(x = fitted_values, y = Life_expectancy)) +
  labs(x = "Fitted Values", y = "Life Expectancy")

#######################################################################################
#######################################################################################
#######################################################################################

#Section 4


#Setting the working directory 
setwd("C:/Users/apr24/OneDrive/Documents/DA/PACD")


#Importing data and data cleaning
lifexpectancydata <- read.csv("~/DA/PACD/lifexpectancydata.csv", header=TRUE)
View(lifexpectancydata)
clean_lifedata <- na.omit(lifexpectancydata)


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


