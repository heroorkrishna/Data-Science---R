# -_- Patients --

patients <- read.csv(choose.files()) # Importing Data
View(patients)
colnames(patients) # Column Names

str(patients) # Structure of the data
summary(patients) # Summary
attach(patients)

# Defining Variables
time <- Followup
event <- Eventtype
group <- Scenario

table(group) # Counting values in each Group

# Exploratory Data Analysis
library(GGally)
ggpairs(patients)
ggplot(data = patients, aes(x = time, fill = Eventtype)) + geom_histogram() + facet_grid(Eventtype ~.) + 
  ggtitle("Distribution of Event-type w.r.t Time")


library(survival)
km_survival <- survfit(Surv(time,event) ~ 1) # Kaplan-Meier non-parametric analysis
summary(km_survival) # Summary of the Model
plot(km_survival, xlab="Time", ylab="Survival Probability") # Visualization
library(survminer)
ggsurvplot(km_survival, data=patients, risk.table = TRUE) # Life Table

km_g_survival <- survfit(Surv(time, event) ~ group) # Kaplan-Meier non-parametric analysis by Group
summary(km_g_survival) # Summary of the model
plot(km_g_survival, xlab="Time", ylab="Survival Probability") # Visualization
ggsurvplot(km_g_survival, data=patients, risk.table = TRUE) # Life Table
