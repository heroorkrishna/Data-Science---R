# -- Forest Fires --

forest_fires <- read.csv(choose.files()) # Importing the data
View(forest_fires)
colnames(forest_fires) # Column Names

library(DataExplorer)
plot_str(forest_fires) 
plot_histogram(forest_fires) # Histogram
attach(forest_fires)
barplot(area) # Barplot

# Normalizing the data

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

# Features that are useful for building the model are Temperature, Rain, Wind and Relative Humidity

foresfires$temp = normalize(foresfires$temp)
foresfires$RH   = normalize(foresfires$RH)
foresfires$wind = normalize(foresfires$wind)
foresfires$rain = normalize(foresfires$rain)

library(caret)
split=0.70 # Specifing Split Ratio
trainIndex <- createDataPartition(forest_fires$area, p=split, list=FALSE) # Partitioning the data

train <- forest_fires[trainIndex,] # Splitting Data into 70% for training
test <- forest_fires[-trainIndex,] # Splitting Data into 30% for testing

library(kernlab)
model_1<-ksvm(size_category ~ temp + rain + wind + RH, data = train,kernel = "vanilladot") # vanilladot Kernel
table(predict(model_1, test) == test$size_category)
mean(predict(model_1, test)==test$size_category) # Accuracy = 73.85%

# Different types of Kernal

model_2 <- ksvm(size_category ~ temp + rain + wind + RH, data = train,kernel = "rbfdot") # rbfdot kernal
mean(predict(model_2, test) == test$size_category) # Accuracy = 75.16%

model_3 <- ksvm(size_category ~ temp + rain + wind + RH, data = train,kernel = "besseldot") # besseldot kernal
mean(predict(model_3, test) == test$size_category) # Accuracy = 73.85%

model_4 <- ksvm(size_category ~ temp + rain + wind + RH, data = train,kernel = "polydot") # polydot kernal
mean(predict(model_4, test) == test$size_category) # Accuracy = 73.85%

model_5 <- ksvm(size_category ~ temp + rain + wind + RH, data = train,kernel = "anovadot") # anovadot kernal
mean(predict(model_5, test) == test$size_category) # Accuracy = 73.85%


# -- Salary Data --


salaries_train <- read.csv(choose.files()) # Importing Train data
salaries_train <- salaries_train[,-c(3,5,7,8,9,13)] # Selecting important feature
View(salaries_train)

salaries_test <- read.csv(choose.files()) # Importing Test data
salaries_test <- salaries_test[,-c(3,5,7,8,9,13)] # Selecting important feature
View(salaries_test)

salaries <- rbind(salaries_train, salaries_test) # Combining Train and Test Data
View(salaries)
colnames(salaries)

library(DataExplorer)
plot_str(salaries)
plot_histogram(salaries) # Histogram
plot_density(salaries) # Density Plot
table(salaries$Salary) 
attach(salaries_train)

library(kernlab)
modl_1<-ksvm(Salary ~ age + workclass + educationno + occupation + capitalgain + capitalloss + hoursperweek, data = salaries_train,kernel = "vanilladot") # vanilladot Kernel
table(predict(modl_1, salaries_test) == salaries_test$Salary)
mean(predict(modl_1, salaries_test)== salaries_test$Salary) # Accuracy = 80.47%

# Different types of Kernal

modl_2 <- ksvm(Salary ~ age + workclass + educationno + occupation + capitalgain + capitalloss + hoursperweek, data = salaries_train,kernel = "rbfdot") # rbfdot kernal
mean(predict(modl_2, salaries_test)== salaries_test$Salary) # Accuracy = 82.43%

modl_3 <- ksvm(Salary ~ age + workclass + educationno + occupation + capitalgain + capitalloss + hoursperweek, data = salaries_train,kernel = "besseldot") # besseldot kernal
mean(predict(modl_3, salaries_test)== salaries_test$Salary) # Accuracy = 77.84%

modl_4 <- ksvm(Salary ~ age + workclass + educationno + occupation + capitalgain + capitalloss + hoursperweek, data = salaries_train,kernel = "polydot") # polydot kernal
mean(predict(modl_4, salaries_test)== salaries_test$Salary) # Accuracy = 80.47%

modl_5 <- ksvm(Salary ~ age + workclass + educationno + occupation + capitalgain + capitalloss + hoursperweek, data = salaries_train,kernel = "anovadot") # anovadot kernal
mean(predict(modl_5, salaries_test)== salaries_test$Salary) # Accuracy = 81.32%
