################################################################################
# Libarys

library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)
library(randomForest)
library(viridisLite)
library(tidyverse)
library(GGally)
library(ggpubr)
library(cluster)
library(gridExtra)
library(ggcorrplot)
library(leaps)
library(plot3D)
library(scatterplot3d)
library(neuralnet)
library(Metrics)
library(tensorflow)
library(forecast)
library(mixtools)
library(corrplot)
library(mgcv)
library(data.table)


################################################################################

# Load the csv file into R
GreySeals <- read.csv("Grey_Seals.csv")
HarbourSeals <- read.csv("Habour_Seals.csv")
Stations <- read.csv("StationDetails.csv")

#### data cleaning #####

data <- read.csv("data2.csv")

# rain only

data_rain <- data[, -c(7:9)]
data_rain <- na.omit(data_rain)

# temp and rain
data_raintemp <- data[, -c(9)]
data_raintemp <- na.omit(data_raintemp)

# all weather

data_allw <- na.omit(data)

#### Models ####

# Convert date column to Date object
data_rain$date <- as.Date(data_rain$date, format="%d/%m/%Y")

## Model 1: Linear Regression
lm_model <- lm(seal_count ~ rainfall, data=data_rain)
summary(lm_model)

## Model 2: Generalized Additive Model (GAM)
gam_model <- gam(seal_count ~ s(rainfall, k=5), data=data_rain, family=poisson(link=log))
summary(gam_model)

## Model 3: Random Forest
set.seed(123)
rf_model <- randomForest(seal_count ~ rainfall, data=data_rain, importance=TRUE)
print(rf_model)
importance(rf_model)

## Model comparison and evaluation
set.seed(123)
models <- list(linear=lm_model, gam=gam_model, rf=rf_model)
folds <- createFolds(data_rain$seal_count, k=10)
cv_results <- lapply(models, function(model) {
  rmse <- vector("numeric", length(folds))
  for (i in 1:length(folds)) {
    test_data <- data_rain[folds[[i]], ]
    train_data <- data_rain[-folds[[i]], ]
    model <- update(model, data=train_data)
    preds <- predict(model, test_data)
    rmse[i] <- sqrt(mean((test_data$seal_count - preds)^2))
  }
  return(mean(rmse))
})
names(cv_results) <- names(models)
cv_results

## Scatter plot with model predictions
data_rain$lm_pred <- predict(lm_model, data_rain)
data_rain$gam_pred <- predict(gam_model, data_rain)
data_rain$rf_pred <- predict(rf_model, data_rain)

ggplot(data_rain) +
  geom_point(aes(x=rainfall, y=seal_count), color='black', alpha=0.5) +
  geom_line(aes(x=rainfall, y=lm_pred, color='red'), size=1) +
  geom_line(aes(x=rainfall, y=gam_pred, color='blue'), size=1) +
  geom_line(aes(x=rainfall, y=rf_pred, color='green'), size=1) +
  xlab("Rainfall (mm)") +
  ylab("Seal Count") +
  ggtitle("Seal Count vs. Rainfall with Model Predictions") +
  scale_color_manual(values=c("red", "blue", "green"), labels=c("Linear", "GAM", "Random Forest")) +
  theme_minimal() +
  theme(legend.position="bottom") +
  labs(color="Model")

#### more test #### 

# site used to convert east and north to lat and long https://www.dancasey.ie/grid2latlon/

# Prepare the map data
ireland_map <- map_data("world", "Ireland")

# Filter only unique seal locations with their respective seal types
unique_locations <- data_rain %>%
  select(lat, long, seal_type) %>%
  distinct()

# Create the map plot
ggplot() +
  geom_polygon(data = ireland_map, aes(x = long, y = lat, group = group), fill = "gray90", color = "gray50") +
  geom_point(data = unique_locations, aes(x = long, y = lat, color = seal_type), size = 2) +
  scale_color_manual(values = c("Grey" = "blue", "Harbour" = "red"), labels = c("Grey Seal", "Harbour Seal")) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("Seal Locations in Ireland by Seal Type")

# Create the scatter plot with rainfall color scale
ggplot() +
  geom_polygon(data = ireland_map, aes(x = long, y = lat, group = group), fill = "gray90", color = "gray50") +
  geom_point(data = data_rain, aes(x = long, y = lat, color = rainfall), size = 2, alpha = 0.7) +
  scale_color_gradient(low = "skyblue", high = "darkblue", name = "Rainfall (mm)") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("Seal Locations and Rainfall in Ireland")

# Create the scatter plot with bubble size representing seal count
ggplot() +
  geom_polygon(data = ireland_map, aes(x = long, y = lat, group = group), fill = "gray90", color = "gray50") +
  geom_point(data = data_rain, aes(x = long, y = lat, size = seal_count, color = rainfall), alpha = 0.7) +
  scale_size_continuous(range = c(1, 5), breaks = c(1, 50, 100), name = "Seal Count") +
  scale_color_gradient(low = "skyblue", high = "darkblue", name = "Rainfall (mm)") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("Seal Count and Rainfall in Ireland")


# Create the hexbin plot
ggplot() +
  geom_polygon(data = ireland_map, aes(x = long, y = lat, group = group), fill = "gray90", color = "gray50") +
  stat_summary_hex(data = data_rain, aes(x = long, y = lat, z = rainfall), fun = "mean", geom="hex", bins = 30, alpha = 0.7) +
  scale_fill_gradient(low = "skyblue", high = "darkblue", name = "Average Rainfall (mm)") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("Seal Density and Average Rainfall in Ireland")


#### GLM model 

# Prepare the data for modeling
model_data <- data_rain %>%
  mutate(location = as.factor(location),
         seal_type = as.factor(seal_type),
         county = as.factor(county))

# Just rain and specific location
# Fit the GLM model with a Poisson distribution
glm_poisson_model <- glm(seal_count ~ rainfall + location + seal_type, data = model_data, family = poisson(link = "log"))

# Summary of the model
summary(glm_poisson_model)

# Using county to be less specific 
# Fit the GLM model using county instead of location

glm_poisson_model2 <- glm(seal_count ~ rainfall + county + seal_type, data = model_data, family = poisson(link = "log"))

# Summary of the model 2
summary(glm_poisson_model2)

# Using data with temp available 
model_data2 <- data_raintemp %>%
  mutate(location = as.factor(location),
         seal_type = as.factor(seal_type),
         county = as.factor(county))

# Fit GLM Model to incorporate temperatures
glm_poisson_model3 <- glm(seal_count ~ rainfall + county + seal_type + mint + maxt, data = model_data2, family = poisson(link = "log"))

# Summary of the model 3
summary(glm_poisson_model3)

# Use super specifc location

# Fit GLM Model to incorporate temperatures
glm_poisson_model4 <- glm(seal_count ~ rainfall + lat + long + seal_type, data = model_data, family = poisson(link = "log"))

# Summary of the model 3
summary(glm_poisson_model4)

# List of Models
# Model 1 - Rainfall and Location
# Model 2 - Rainfall and county
# Model 3 - Use Temp and County
# Model 4 - Use Lat and Long as location factors


#### Mapping the models

# Modle 1
predicted_seal_count <- predict(glm_poisson_model, model_data, type = "response")

plotmodel1 <- ggplot(model_data, aes(x = rainfall, y = seal_count)) +
  geom_point(colour = "orange") +
  geom_line(aes(y = predicted_seal_count), color = "blue", linewidth = 1) +
  theme_minimal() +
  labs(x = "Rainfall (mm)", y = "Seal Count", title = "Model 1 - Weather Station Location")

plotmodel1

# Model 2
predicted_seal_count2 <- predict(glm_poisson_model2, model_data, type = "response")

plotmodel2 <- ggplot(model_data, aes(x = rainfall, y = seal_count)) +
  geom_point(colour = "orange") +
  geom_line(aes(y = predicted_seal_count2), color = "blue", linewidth = 1) +
  theme_minimal() +
  labs(x = "Rainfall (mm)", y = "Seal Count", title = "Model 2 - County")

plotmodel2

# Model 4
predicted_seal_count4 <- predict(glm_poisson_model4, model_data, type = "response")

plotmodel4 <- ggplot(model_data, aes(x = rainfall, y = seal_count)) +
  geom_point(colour = "orange") +
  geom_line(aes(y = predicted_seal_count4), color = "blue", linewidth = 1) +
  theme_minimal() +
  labs(x = "Rainfall (mm)", y = "Seal Count", title = "Model 4 - Lat and Long")

plotmodel4

# Display the plots 1,2 & 4
grid.arrange(plotmodel1, plotmodel2, plotmodel4, ncol = 3)


# Other Plots
# Seals per county

# Generate predicted seal counts for each model
model_data$predicted_seal_count1 <- predict(glm_poisson_model, model_data, type = "response")
model_data$predicted_seal_count2 <- predict(glm_poisson_model2, model_data, type = "response")
model_data$predicted_seal_count4 <- predict(glm_poisson_model4, model_data, type = "response")

# Calculate average predicted seal counts for each county and model
avg_predicted_seal_count1 <- aggregate(predicted_seal_count1 ~ county, data = model_data, FUN = mean)
avg_predicted_seal_count2 <- aggregate(predicted_seal_count2 ~ county, data = model_data, FUN = mean)
avg_predicted_seal_count4 <- aggregate(predicted_seal_count4 ~ county, data = model_data, FUN = mean)

# Visuals 

# Create bar plots for each model's predictions
seal_plot1 <- ggplot(avg_predicted_seal_count1, aes(x = reorder(county, -predicted_seal_count1), y = predicted_seal_count1)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(x = "County", y = "Average Predicted Seal Count", title = "Model 1 - Weather Station Location") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

seal_plot2 <- ggplot(avg_predicted_seal_count2, aes(x = reorder(county, -predicted_seal_count2), y = predicted_seal_count2)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  theme_minimal() +
  labs(x = "County", y = "Average Predicted Seal Count", title = "Model 2 - County") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

seal_plot4 <- ggplot(avg_predicted_seal_count4, aes(x = reorder(county, -predicted_seal_count4), y = predicted_seal_count4)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  theme_minimal() +
  labs(x = "County", y = "Average Predicted Seal Count", title = "Model 4 - Lat and Long") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plots side by side
grid.arrange(seal_plot1, seal_plot2, seal_plot4, ncol = 3)

# Map with other weather effects
# Temp

ggplot() +
  geom_polygon(data = ireland_map, aes(x = long, y = lat, group = group), fill = "gray90", color = "gray50") +
  geom_point(data = data_raintemp, aes(x = long, y = lat, size = seal_count, color = mint), alpha = 0.7) +
  scale_size_continuous(range = c(1, 5), breaks = c(1, 50, 100), name = "Seal Count") +
  scale_color_gradient(low = "yellow", high = "red", name = "Temperture (c)") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("Seal Count and Temperture in Ireland")

# Wind

ggplot() +
  geom_polygon(data = ireland_map, aes(x = long, y = lat, group = group), fill = "gray90", color = "gray50") +
  geom_point(data = data_allw, aes(x = long, y = lat, size = seal_count, color = wdsp), alpha = 0.7) +
  scale_size_continuous(range = c(1, 5), breaks = c(1, 50, 100), name = "Seal Count") +
  scale_color_gradient(low = "lightgreen", high = "darkgreen", name = "Wind speed") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("Seal Count and Windpseed in Ireland")
