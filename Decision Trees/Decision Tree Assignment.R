# -- Party Package --

data("iris") # Importing Iris Data
View(iris)
boxplot(iris)

iris_setosa <- iris[iris$Species=="setosa",] # 50 values
iris_versicolor <- iris[iris$Species=="versicolor",] # 50 values
iris_virginica <- iris[iris$Species=="virginica",] # 50 values
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,]) # Splitting 50% Data into Training Set
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,]) # Splitting 50% Data into Testing Set

library(party) # Library required to build the model
iris_tree <- ctree(Species ~ ., data = iris_train) # Tree Building
plot(iris_tree) # Graphical Visualization of the Tree
table(predict(iris_tree), iris_test$Species) # Expected Values v/s Predicted Values

mean(iris_train$Species == predict(iris_tree,iris_test)) # Accuracy of the model is 94.66%


# -- Company Sales Dataset --

companies <- read.csv(choose.files()) # Import Data
View(companies)
boxplot(companies)
summary(companies) # Summary
colnames(companies)
library(caret)
split=0.70 # Specifing Split Ratio
trainIndex <- createDataPartition(companies$Sales, p=split, list=FALSE) # Partitioning the data based on the Glass Type

company_train <- companies[trainIndex,] # Splitting Data into 70% for training
company_test <- companies[-trainIndex,] # Splitting Data into 30% for testing

library(rpart)
model_sales <- rpart(Sales ~ .,data = company_train) # Building Tree
plot(model_sales)
text(model_sales)
summary(model_sales)
pred_sales <- predict(model_sales,company_test) # Model Prediction
rmse_sales <- sqrt(mean((pred_sales-company_test$Sales)^2))
rmse_sales

plot(pred_sales,company_test$Sales)
cor(pred_sales,company_test$Sales) # Correlation


# -- Fruad Check Data --

fruads <- read.csv(file.choose()) # Importing Data
colnames(fruads)
decision <- NULL
decision <- ifelse(fruads$Taxable.Income <= 30000,1,0) # Condition for the decision "Risky"=1 and "Good"=0
fruads[,"decision"] <- decision

# Datatype Convertion
fruads$Undergrad <- as.factor(fruads$Undergrad)
fruads$Marital.Status <- as.factor(fruads$Marital.Status)
fruads$Urban <- as.factor(fruads$Urban)
fruads$decision <- as.factor(fruads$decision)

risky <- fruads[fruads$decision == "1",] 
good <- fruads[fruads$decision == "0",]

data_train <- rbind(risky[1:93,], good[1:357,]) # Training Data
data_test <- rbind(risky[94:124,], good[357:476,]) # Testing Data

library(C50)
trained_model <- C5.0(data_train[,-c(7)], data_train$decision) # Tree Building
plot(trained_model)
mean(data_train$decision == predict(trained_model, data_train)) # Accuracy
pred_test <- predict(trained_model, newdata = data_test)

mean(pred_test == data_test$decision)

library(gmodels)
CrossTable(data_test$decision, pred_test) # Cross Table to compare Data
