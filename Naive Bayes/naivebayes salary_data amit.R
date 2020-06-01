########naivebayes ################


install.packages("naivebayes")
install.packages("ggplot2")
install.packages("caret")
install.packages("psych")
install.packages("e1071")


library(naivebayes)
library(ggplot2)

library(caret)


library(psych)

library(e1071)

train_sal <- read.csv(file.choose())
str(train_sal)

train_sal$educationno <- as.factor(train_sal$educationno)
class(train_sal)

test_sal <- read.csv(file.choose())
str(test_sal)

View(test_sal)
test_sal$educationno <- as.factor(test_sal$educationno)
class(test_sal)
## [1] "data.frame"
#Visualization 
# Plot and ggplot 
ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$age, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")


plot(train_sal$workclass,train_sal$Salary)


plot(train_sal$education,train_sal$Salary)


plot(train_sal$educationno,train_sal$Salary)


plot(train_sal$maritalstatus,train_sal$Salary)


plot(train_sal$occupation,train_sal$Salary)


plot(train_sal$relationship,train_sal$Salary)


plot(train_sal$race,train_sal$Salary)


plot(train_sal$sex,train_sal$Salary)


ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$capitalgain, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")


ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$capitalloss, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")


ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$hoursperweek, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")


plot(train_sal$native,train_sal$Salary)


#Density Plot 

ggplot(data=train_sal,aes(x = train_sal$age, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


ggtitle("Age - Density Plot")

ggplot(data=train_sal,aes(x = train_sal$workclass, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


ggtitle("Workclass Density Plot")

ggplot(data=train_sal,aes(x = train_sal$education, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


ggtitle("education Density Plot")

ggplot(data=train_sal,aes(x = train_sal$educationno, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


ggtitle("educationno Density Plot")

ggplot(data=train_sal,aes(x = train_sal$maritalstatus, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


ggtitle("maritalstatus Density Plot")

ggplot(data=train_sal,aes(x = train_sal$occupation, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


ggtitle("occupation Density Plot")

ggplot(data=train_sal,aes(x = train_sal$sex, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


ggtitle("sex Density Plot")

ggplot(data=train_sal,aes(x = train_sal$relationship, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


ggtitle("Relationship Density Plot")

ggplot(data=train_sal,aes(x = train_sal$race, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


ggtitle("Race Density Plot")

ggplot(data=train_sal,aes(x = train_sal$capitalgain, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


ggtitle("Capitalgain Density Plot")

ggplot(data=train_sal,aes(x = train_sal$capitalloss, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


ggtitle("Capitalloss Density Plot")

ggplot(data=train_sal,aes(x = train_sal$hoursperweek, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


ggtitle("Hoursperweek Density Plot")

ggplot(data=train_sal,aes(x = train_sal$native, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')



ggtitle("native Density Plot")

# Naive Bayes Model 
Model <- naiveBayes(train_sal$Salary ~ ., data = train_sal)
Model
## 
## Naive Bayes Classifier for Discrete Predictors

Model_pred <- predict(Model,test_sal)
mean(Model_pred==test_sal$Salary)

confusionMatrix(Model_pred,test_sal$Salary)
## Confusion Matrix and Statistics