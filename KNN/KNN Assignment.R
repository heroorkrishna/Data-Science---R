# -- Zoo Dataset --

zoo <- read.csv(file.choose()) # Importing Zoo Dataset
View(zoo)
str(zoo) 
attach(zoo)

zoo$type <- as.factor(type)
zoo <- zoo[,-1] # Removing First column, due to its less importance
View(zoo)

zoo_inputs <- zoo[,-c(17)] # Selecting Inputs
zoo_inputs
# User created Normal Function
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

zoo_inputs <- as.data.frame(lapply(zoo_inputs, norm)) # Normalizing the Dataset
View(zoo_inputs)
summary(zoo_inputs)

zoo$type <- factor(zoo$type, levels = c("1","2","3","4","5","6","7"),
                   labels = c("type1","type2","type3","type4","type5","type6","type7")) # Renaming Y values


zoo_train <- zoo_inputs[1:70,] # Selecting 70% of data for Training
zoo_test <- zoo_inputs[71:101,] # Selecting rest of the 30% of data for Testing

zoo_train_lbs <- zoo[1:70,17] # Selecting Output Training labels
zoo_test_lbs <- zoo[71:101,17] # Selecting Output Testing labels


sqrt_n = ceiling(round(sqrt(nrow(zoo_train)))) # Finding optimal K value using square root of the total number of observations

library(class)
model_knn <- knn(train = zoo_train,test = zoo_test, cl= zoo_train_lbs,k=sqrt_n) # Model Building

library(gmodels)
CrossTable( x =  zoo_test_lbs, y = model_knn) # Cross table of predicted values and actual values
table(zoo_test_lbs)

#-----------------------------------------------------------------------------------------------------------------------------------

# -- Glass Dataset --

glass <- read.csv(file.choose()) # Importing Glass Dataset
View(glass)
str(glass) 
attach(glass)

glass$type <- as.factor(type)

glass_inputs <- glass[,-c(10)] # Selecting Inputs

# User created Normal Function
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

glass_inputs <- as.data.frame(lapply(glass_inputs, norm)) # Normalizing the Dataset using user created function
View(glass_inputs)
summary(glass_inputs)

glass$Type <- factor(glass$Type, levels = c("1","2","3","4","5","6","7"),
                   labels = c("type1","type2","type3","type4","type5","type6","type7")) # Renaming Y values

library(caret)
split=0.70 # Specifing Split Ratio
trainIndex <- createDataPartition(glass$Type, p=split, list=FALSE) # Partitioning the data based on the Glass Type

glass_train <- glass_inputs[ trainIndex,] # Splitting 70% of the data for Training
glass_test <- glass_inputs[-trainIndex,] # Splitting rest of the of data (i.e 30%) for Testing

glass_train_lbs <- glass[ trainIndex,10] # Selecting Output Training labels
glass_test_lbs <- glass[-trainIndex,10] # Selecting Output Testing labels
 
table(glass_train_lbs)
table(glass_test_lbs) 

sqrt_n = ceiling(round(sqrt(length(glass_train)))) # Finding optimal K value using square root of the total number of observations

library(class)
model_knn <- knn(train = data.frame(glass_train),test = data.frame(glass_test), cl= glass_train_lbs,k=sqrt_n) # Model Building

library(gmodels)
CrossTable( x =  glass_test_lbs, y = model_knn) # Cross table of predicted values and actual values