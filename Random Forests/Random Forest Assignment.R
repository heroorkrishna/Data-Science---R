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

companies<- read.csv(choose.files()) # Importing companies dataset
View(companies)
colnames(companies)
mean_sales <- mean(companies$Sales) # Mean of the Total Sales

Sales_Result <- NULL
Sales_Result <- ifelse(companies$Sales > mean_sales,1,0) # Condition
companies[,"Sales_Result"] <- Sales_Result # Creating a new column

# Converting datatypes
companies$ShelveLoc <- as.factor(companies$ShelveLoc)
companies$Urban <- as.factor(companies$Urban)
companies$US <- as.factor(companies$US)
companies$Sales_Result <- as.factor(companies$Sales_Result)

sales_high <- companies[companies$Sales_Result == "1",] 
sales_low <- companies[companies$Sales_Result == "0",]

data_train <- rbind(sales_high[1:150,], sales_low[1:150,]) # Building Train Data
data_test <- rbind(sales_high[151:199,], sales_low[151:201,]) # Building Test Data

library(randomForest)
company_tree <- randomForest(Sales_Result~.,data=data_train, na.action=na.roughfix, importance=TRUE) # Building a Forest of Decision Trees

pred_test <- predict(company_tree,newdata=data_test) # Predicting test data 
mean(pred_test == data_test$Sales_Result) # Accuracy

confusionMatrix(data_test$Sales_Result, pred_test) # Confusion Matrix

plot(company_tree,lwd=2) # Vizualization
legend("topright", colnames(company_tree$err.rate),col=1:4,cex=0.8,fill=1:4)


# -- Fruad Data --

fruads <- read.csv(choose.files()) # Importing the Fruads Data
colnames(fruads)
decision <- NULL
decision <- ifelse(fruads$Taxable.Income<=30000,1,0) # Condition
fruads[,"decision"] <- decision # New column

#Type Convertion
fruads$Undergrad <- as.factor(fruads$Undergrad)
fruads$Marital.Status <- as.factor(fruads$Marital.Status)
fruads$Urban <- as.factor(fruads$Urban)
fruads$decision <- as.factor(fruads$decision)

fraud_risky <- fruads[fruads$decision == "1",] 
fraud_good <- fruads[fruads$decision == "0",]

data_train <- rbind(fraud_risky[1:93,], fraud_good[1:357,]) # Train Data
data_test <- rbind(fraud_risky[94:124,], fraud_good[357:476,]) # Test Data

library(randomForest)
fit.forest <- randomForest(decision~., data=data_train, na.action=na.roughfix, importance=TRUE) # Building a Forest of Decision Trees

library(caret)
pred_test <- predict(fit.forest,newdata=data_test) # Prediction test data

mean(pred_test == data_test$decision) # Accuracy of Training Data
confusionMatrix(data_test$decision, pred_test) # Confusion Matrix of Test Data

plot(fit.forest,lwd=2) # Visualization
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)
