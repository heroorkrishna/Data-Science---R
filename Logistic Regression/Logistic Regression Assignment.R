# -- Affairs Dataset --

library(AER)
library(plyr)

affairs <- data("Affairs")
View(Affairs)

affairs1 <- Affairs
summary(affairs1)

table(affairs1$affairs)

affairs1$ynaffairs[affairs1$affairs > 0] <- 1
affairs1$ynaffairs[affairs1$affairs == 0] <- 0
affairs1$gender <- as.factor(revalue(Affairs$gender,c("male"=1, "female"=0)))
affairs1$children <- as.factor(revalue(Affairs$children,c("yes"=1, "no"=0)))

View(affairs1)

colnames(affairs1)

class(affairs1)
attach(affairs1)

model <- glm(ynaffairs ~ factor(gender) + age+ yearsmarried+ factor(children) + religiousness+
               education+occupation+rating, data = affairs1,family = "binomial")
summary(model)
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model))

prob <- predict(model,affairs1,type="response")
summary(model)

confusion<-table(prob>0.5,affairs1$ynaffairs)
confusion

Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 76.53% accurate model

pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
affairs1[,"prob"] <- prob
affairs1[,"pred_values"] <- pred_values
affairs1[,"yes_no"] <- yes_no

View(affairs1[,c(1,9:11)])

table(affairs1$ynaffairs,affairs1$pred_values)

library(ROCR)

rocrpred<-prediction(prob,affairs1$ynaffairs)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
str(rocrperf)

rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)

rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))

View(rocr_cutoff)



# -- Bank Data --

banks <- read.csv(choose.files()) # Import banks dataset after cleaning it and seperating it into correct columns
View(banks)
sum(is.na(banks)) # Checking for NA values in the dataset. No NA values found, so no need top remove them
attach(banks)
colnames(banks)
summary(banks) # Summary

# Changing values for caterigorical features
library(plyr)
banks$marital <- as.factor(revalue(banks$marital, c("single" = 1, "married" = 2, "divorced" = 3)))
banks$education <- as.factor(revalue(banks$education, c("unknown" = 0, "primary" = 1, "secondary" = 2, "tertiary" = 3)))
banks$default <- as.factor(revalue(banks$default, c("yes" = 1, "no" = 0)))
banks$housing <- as.factor(revalue(banks$housing, c("yes" = 1, "no" = 0)))
banks$loan <- as.factor(revalue(banks$loan, c("yes" = 1, "no" = 0)))
banks$contact <- as.factor(revalue(banks$contact, c("unknown" = 0, "telephone" = 1, "cellular" = 2)))
banks$month <- as.factor(revalue(banks$month, c("jan" = 1, "feb" = 2, "mar" = 3, "apr" = 4, "may" = 5, "jun" = 6, "jul" = 7, "aug" = 8, "sep" = 9, "oct" = 10, "nov" = 11, "dec" = 12)))
banks$poutcome <- as.factor(revalue(banks$poutcome, c("failure" = -1, "unknown" = 0, "success" = 1, "other" = 2)))
banks$y <- as.factor(revalue(banks$y, c("yes" = 1, "no" = 0)))
View(banks)

banks_main <- banks[,-2] # Taking all independent variables except "job description" for building model
View(banks_main)

model <- glm(y~., data=banks_main, family = "binomial") # Building Model based on all independent variables except "job description"

exp(coef(model)) # Exponential of Coefficient of the built model

prob <- predict(model, banks_main, type="response")
summary(model)

confusion <- table(prob>0.5, banks_main$y) # Confusion Matrix based on threshold greater than 0.5
confusion

accuracy <- sum(diag(confusion)/sum(confusion)) # Accuracy of the built model
accuracy # 90% Accurate model

pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5, 1, 0)
yes_no <- ifelse(prob>=0.5, "yes", "no")

# Creating new column to store the above values
banks_main[,"prob"] <- prob
banks_main[,"pred_values"] <- pred_values
banks_main[,"yes_no"] <- yes_no

View(banks_main[,c(15:19)])

table(banks_main$y,banks_main$pred_values)

library(ROCR)
rocrpred<-prediction(prob,banks_main$y)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
str(rocrperf)

rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR)) # Sorting data frame with respect to tpr in decreasing order 

View(rocr_cutoff)
