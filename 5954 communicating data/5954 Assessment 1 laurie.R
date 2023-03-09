##5954 Group Presentation

Life.Expectancy.Data1 <- read.csv("~/Data Analytics/5954 data/Life Expectancy Data.csv")
View(Life.Expectancy.Data)

Life.Expectancy.Data<-na.omit(Life.Expectancy.Data1)

#Library

library(ggridges)
library(ggplot2)
library(forcats)
library(tidyverse)
library(hexbin)
library(dplyr)
library(caret)
library(hnp)

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



