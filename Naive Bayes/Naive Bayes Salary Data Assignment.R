# -- Salary Dataset --

salary_train <- read.csv(choose.files()) # Importing Train Data
salary_test <- read.csv(choose.files()) # Importing Test Data

View(salary_train) # Display Train Data
View(salary_test) # Display Test Data

str(salary_train) # Structure of Train Data
str(salary_test) # Structure of Test Data

summary(salary_train) # Summary of Train Data
summary(salary_test) # Summary of Train Data

colnames(salary_train) # Column Names
attach(salary_train) # Attaching Training Dataset

# Exploratory Data Analysis
plot(workclass, Salary) # Workclass versus Salary

plot(education, Salary) # Education versus Salary

plot(maritalstatus, Salary) # Maritalstatus versus Salary

plot(occupation, Salary) # Occupation versus Salary

plot(relationship, Salary) # Relationship versus Salary

plot(race, Salary) # Race versus Salary

plot(sex, Salary) # Sex versus Salary

plot(native, Salary) # Native versus Salary

library(ggplot2)
ggplot(data = salary_train, aes(x = Salary, y = age, fill = Salary)) + geom_boxplot() + ggtitle("Box Plot - Age") # Age versus Salary

ggplot(data = salary_train, aes(x = age, fill = Salary)) + geom_density() + ggtitle("Density Plot - Age")

ggplot(data = salary_train, aes(x = workclass, fill = Salary)) + geom_density() + ggtitle("Density Plot - Workclass")

ggplot(data = salary_train, aes(x = education, fill = Salary)) + geom_density() + ggtitle("Density Plot - Education")

ggplot(data = salary_train, aes(x = occupation, fill = Salary)) + geom_density() + ggtitle("Density Plot - Occupation")

ggplot(data = salary_train, aes(x = capitalgain, fill = Salary)) + geom_density() + ggtitle("Density Plot - Capitalgain")

ggplot(data = salary_train, aes(x = capitalloss, fill = Salary)) + geom_density() + ggtitle("Density Plot - Capitalloss")

ggplot(data = salary_train, aes(x = hoursperweek, fill = Salary)) + geom_density() + ggtitle("Density Plot - Hours per week")


library(e1071)
nb_model <- naiveBayes(salary_train$Salary ~ ., data = salary_train) # Naive Bayes Model Building

nb_model # Naive Bayes Model

nb_model_pred <- predict(nb_model, salary_test) # Model Prediction with Test Data

mean(nb_model_pred == salary_test$Salary) # Accuracy of the built model = 81.93%

library(caret)
confusionMatrix(nb_model_pred, salary_test$Salary) # Confusion Matrix

