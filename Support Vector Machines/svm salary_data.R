#Preparing a classification model using SVM for salary data 
install.packages("kernlab")
library(kernlab)
install.packages("caret")
library(caret)
install.packages("plyr")
library(plyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("psych")
library(psych)
install.packages("e1071")
library(e1071)
# Data(Train)
train_sal <- read.csv(file.choose())
str(train_sal)
View(train_sal)
train_sal$educationno <- as.factor(train_sal$educationno)
class(train_sal)
# Data(Test)
test_sal <- read.csv(file.choose())
str(test_sal)
View(test_sal)
test_sal$educationno <- as.factor(test_sal$educationno)
class(test_sal)
ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$age, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$capitalgain, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
#Density Plot 

ggplot(data=train_sal,aes(x = train_sal$age, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=train_sal,aes(x = train_sal$workclass, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
# Building model 
#install.packages("kernlab")
#library(kernlab)

model1<-ksvm(train_sal$Salary~., 
             data= train_sal, kernel = "vanilladot")
model1
Salary_prediction <- predict(model1, test_sal)

table(Salary_prediction,test_sal$Salary)
agreement <- Salary_prediction == test_sal$Salary
table(agreement)
prop.table(table(agreement))

# Different types of kernels 

# kernel = rfdot 
model_rfdot<-ksvm(train_sal$Salary~., 
                  data= train_sal,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=test_sal)
mean(pred_rfdot==test_sal$Salary) 

# kernel = vanilladot
model_vanilla<-ksvm(train_sal$Salary~., 
                    data= train_sal,kernel = "vanilladot")

##  Setting default kernel parameters
pred_vanilla<-predict(model_vanilla,newdata=test_sal)
mean(pred_vanilla==test_sal$Salary) 