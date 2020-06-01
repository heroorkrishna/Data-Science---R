#install.packages("NeuralNetTools")
library(nnet)
#install.packages("neuralnet")
library(neuralnet)
#install.packages("plyr")
library(plyr)
Startups <- read.csv(file.choose())
View(Startups)
class(Startups)
Startups$State <- as.numeric(revalue(Startups$State,c("New York"="0", "California"="1", "Florida"="2")))
str(Startups)
Startups <- as.data.frame(Startups)
attach(Startups)

# Exploratory data Analysis :

plot(R.D.Spend, Profit)
plot(Administration, Profit)
plot(Marketing.Spend, Profit)
plot(State, Profit)
windows()
# Find the correlation between Output (Profit) & inputs (R.D Spend, Administration, Marketing, State) - SCATTER DIAGRAM
pairs(Startups)
# Correlation coefficient - Strength & Direction of correlation
cor(Startups)
summary(Startups)
# Confirms on the different scale and demands normalizing the data.
# Apply Normalization technique to the whole dataset :

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
Startups_norm<-as.data.frame(lapply(Startups,FUN=normalize))
summary(Startups_norm$Profit)
# Normalized form of profit
summary(Startups$profit) 
# Orginal profit value
# Data Partition 
set.seed(123)
ind <- sample(2, nrow(Startups_norm), replace = TRUE, prob = c(0.7,0.3))
Startups_train <- Startups_norm[ind==1,]
startups_test  <- Startups_norm[ind==2,]


# Creating a neural network model on training data


startups_model <- neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend+State,data = Startups_train)
str(startups_model)
plot(startups_model, rep = "best")
summary(startups_model)
par(mar = numeric(4), family = 'serif')
# Evaluating model performance

set.seed(12323)
model_results <- compute(startups_model,startups_test[1:4])
predicted_profit <- model_results$net.result

# Predicted profit Vs Actual profit of test data.
cor(predicted_profit,startups_test$Profit)
str_max <- max(Startups$Profit)
str_min <- min(Startups$Profit)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualProfit_pred <- unnormalize(predicted_profit,str_min,str_max)
head(ActualProfit_pred)

# Improve the model performance :
set.seed(12345)
Startups_model2 <- neuralnet(Profit~R.D.Spend+Administration +Marketing.Spend+State,data = Startups_train, hidden = 2)
plot(Startups_model2 ,rep = "best")
summary(Startups_model2)
model_results2<-compute(Startups_model2,startups_test[1:4])
predicted_Profit2<-model_results2$net.result
cor(predicted_Profit2,startups_test$Profit)
plot(predicted_Profit2,startups_test$Profit)

