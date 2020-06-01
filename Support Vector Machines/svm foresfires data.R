#Prepare support vector machines model for classifying the area under fire for foresfires data
#install.packages("kernlab")
library(kernlab)
#install.packages("caret")
library(caret)
#install.packages("plyr")
library(plyr)
# Read the data
foresfires <- read.csv(file.choose())
View(foresfires)
class(foresfires)
str(foresfires)
barplot(foresfires$area)
hist(foresfires$area)
FF1 <- mutate(foresfires, y = log(area+1))  # default is to the base e, y is lower case
hist(FF1$y)
# Confirms on the different scale and demands normalizing the data.
summary(foresfires)
# Prediction of Forest fires requires only prediction from 
# temperature, rain, relative humidity and wind speed

# Apply Normalization technique to the whole dataset :

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
foresfires$temp = normalize(foresfires$temp)
foresfires$RH   = normalize(foresfires$RH)
foresfires$wind = normalize(foresfires$wind)
foresfires$rain = normalize(foresfires$rain)
# We need to tweak this as a classification problem.lets base out the Size using this criteria :

attach(foresfires)
# Data Partition 
set.seed(123)
ind <- sample(2, nrow(foresfires), replace = TRUE, prob = c(0.7,0.3))
foresfires_train <- foresfires[ind==1,]
foresfires_test  <- foresfires[ind==2,]
# kvsm() function uses gaussian RBF kernel 
# Building model 
library(kernlab)
model1<-ksvm(size_category~temp+rain+wind+RH, 
             data= foresfires_train,kernel = "vanilladot")
model1
Area_pred <- predict(model1, foresfires_test)

table(Area_pred,foresfires_test$size_category)
agreement <- Area_pred == foresfires_test$size_category
table(agreement)
prop.table(table(agreement))
# Different types of kernels 
# kernel = rfdot 
model_rfdot<-ksvm(size_category~temp+rain+wind+RH, 
                  data= foresfires_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=foresfires_test)
mean(pred_rfdot==foresfires_test$size_category) 

# kernel = vanilladot
model_vanilla<-ksvm(size_category~temp+rain+wind+RH, 
                    data= foresfires_train,kernel = "vanilladot")

##  Setting default kernel parameters
pred_vanilla<-predict(model_vanilla,newdata=foresfires_test)
mean(pred_vanilla==foresfires_test$size_category) 

# kernal = besseldot
model_besseldot<-ksvm(size_category~temp+rain+wind+RH, 
                      data= foresfires_train,kernel = "besseldot")

##  Setting default kernel parameters
pred_bessel<-predict(model_besseldot,newdata=foresfires_test)
mean(pred_bessel==foresfires_test$size_category) 


