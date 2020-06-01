# -- 50 Startups --

start_ups <- read.csv(choose.files())
View(start_ups)
class(start_ups)

library(plyr)
start_ups$State <- as.numeric(revalue(start_ups$State,c("New York"="1", "California"="2", "Florida"="3")))
str(start_ups)
start_ups <- as.data.frame(start_ups)
attach(start_ups)

# Exploratory data Analysis
library(DataExplorer)

plot_str(start_ups)
plot_histogram(start_ups)
plot_correlation(start_ups)
plot_density(start_ups)

plot(R.D.Spend, Profit)
plot(Administration, Profit)
plot(Marketing.Spend, Profit)
plot(State, Profit)

pairs(start_ups)

cor(start_ups) # Correlation between all feature
summary(start_ups) # Summary

# Normalize Function
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

startup_norm <- as.data.frame(lapply(start_ups,FUN=normalize)) # Normalizing the data
View(startup_norm)

summary(start_ups$Profit) # Summary of original profit
summary(startup_norm$Profit) # Summary of profit after normalizing

library(caret)
split=0.70 # Specifing Split Ratio
trainIndex <- createDataPartition(startup_norm$Profit, p=split, list=FALSE) # Partitioning the data based on the Profit

train <- startup_norm[ trainIndex,] # Splitting 70% of the data for Training
test <- startup_norm[-trainIndex,] # Splitting rest of the of data (i.e 30%) for Testing

library(neuralnet)
nn_model <- neuralnet(Profit ~ .-Profit, data = train) # Building the Neural Network
str(nn_model) # Structure of the model
plot(nn_model) # Visualization
summary(nn_model) # Summary
profit_pred <- (compute(nn_model, test[1:4])$net.result) # Prediction
cor(profit_pred, test$Profit) # Correlation between Profit of the built Neural Network and test dataset
plot(profit_pred, test$Profit)

nnn_model <- neuralnet(Profit ~ .-Profit, data = train, hidden = 4) # Building new Neural Network with 4 hidden layers
str(nnn_model) # Structure of the model
plot(nnn_model) # Visualization
summary(nnn_model) # Summary
nprofit_pred <- (compute(nnn_model, test[1:4])$net.result) # Prediction
cor(nprofit_pred, test$Profit) # Correlation between Profit of the built Neural Network and test dataset
plot(nprofit_pred, test$Profit)

# Thus from  two neural networks, the one with the hidden layer has higher correlation value.
# Indicating that the hidden layer neural network is a better model than the other one.



# -- Concrete Data --

concrete <- read.csv(choose.files())
View(concrete)
class(concrete)

colnames(concrete)
attach(concrete)

# Exploratory data Analysis
library(DataExplorer)

plot_str(concrete)
plot_histogram(concrete)
plot_correlation(concrete)
plot_density(concrete)

plot(cement, strength)
plot(slag, strength)
plot(ash, strength)
plot(water, strength)
plot(superplastic, strength)
plot(coarseagg, strength)
plot(fineagg, strength)
plot(age, strength)

pairs(concrete)

cor(concrete) # Correlation between all feature
summary(concrete) # Summary

# Normalize Function
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

concrete_norm <- as.data.frame(lapply(concrete,FUN=normalize)) # Normalizing the data
View(concrete_norm)

summary(concrete$strength) # Summary of original strength
summary(concrete_norm$strength) # Summary of strength after normalizing

library(caret)
split=0.70 # Specifing Split Ratio
trainIndex <- createDataPartition(concrete_norm$strength, p=split, list=FALSE) # Partitioning the data based on the strength

c_train <- concrete_norm[ trainIndex,] # Splitting 70% of the data for Training
c_test <- concrete_norm[-trainIndex,] # Splitting rest of the of data (i.e 30%) for Testing

library(neuralnet)
cnn_model <- neuralnet(strength ~ .-strength, data = c_train) # Building the Neural Network
str(cnn_model) # Structure of the model
plot(cnn_model) # Visualization
summary(cnn_model) # Summary
strength_pred <- (compute(cnn_model, c_test[1:8])$net.result) # Prediction
cor(strength_pred, c_test$strength) # Correlation between strength of the built Neural Network and test dataset
plot(strength_pred, c_test$strength)

ncnn_model <- neuralnet(strength ~ .-strength, data = c_train, hidden = 5) # Building new Neural Network with 5 hidden layers
str(ncnn_model) # Structure of the model
plot(ncnn_model) # Visualization
summary(ncnn_model) # Summary
nstrength_pred <- (compute(ncnn_model, c_test[1:8])$net.result) # Prediction
cor(nstrength_pred, c_test$strength) # Correlation between strength of the built Neural Network and test dataset
plot(nstrength_pred, c_test$strength)

# Thus from  two neural networks, the one with the hidden layer has higher correlation value.
# Indicating that the hidden layer neural network is a better model than the other one.



# -- Forest Fires --

fires <- read.csv(choose.files())
fires <- fires[,3:11]
View(fires)
colnames(fires)
attach(fires)

# Exploratory data Analysis
library(DataExplorer)

plot_str(fires)
plot_histogram(fires)
plot_correlation(fires)
plot_density(fires)

plot(FFMC, area)
plot(DMC, area)
plot(DC, area)
plot(ISI, area)
plot(temp, area)
plot(RH, area)
plot(wind, area)
plot(rain, area)

pairs(fires)

cor(fires) # Correlation between all feature
summary(fires) # Summary

# Normalize Function
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

fires_norm <- as.data.frame(lapply(fires,FUN=normalize)) # Normalizing the data
View(fires_norm)

summary(fires$area) # Summary of original area
summary(fires_norm$area) # Summary of area after normalizing

library(caret)
split=0.70 # Specifing Split Ratio
trainIndex <- createDataPartition(fires_norm$area, p=split, list=FALSE) # Partitioning the data based on the area

fire_train <- fires_norm[ trainIndex,] # Splitting 70% of the data for Training
fire_test <- fires_norm[-trainIndex,] # Splitting rest of the of data (i.e 30%) for Testing

library(neuralnet)
fnn_model <- neuralnet(area ~ .-area, data = fire_train) # Building the Neural Network
str(fnn_model) # Structure of the model
plot(fnn_model) # Visualization
summary(fnn_model) # Summary
area_pred <- (compute(fnn_model, fire_test[1:8])$net.result) # Prediction
cor(area_pred, test$area) # Correlation between area of the built Neural Network and test dataset
plot(area_pred, test$area)


nfnn_model <- neuralnet(area ~ .-area, data = fire_train, hidden = 1) # Building new Neural Network with 5 hidden layers
str(nfnn_model) # Structure of the model
plot(nfnn_model) # Visualization
summary(nfnn_model) # Summary
narea_pred <- (compute(nfnn_model, fire_test[1:8])$net.result) # Prediction
cor(narea_pred, fire_test$area) # Correlation between area of the built Neural Network and test dataset
plot(narea_pred, fire_test$area)

# Thus from  two neural networks, the one with the hidden layer has lower correlation value.
# Indicating that the hidden layer neural network is not a better model than the other one.
