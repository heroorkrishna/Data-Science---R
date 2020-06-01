# -- Airlines Data --

library(readxl)
airlines <- read_excel(choose.files()) # Importing Data
View(airlines)
colnames(airlines) # Column Names

library(tseries)
airlines_ts <- ts(airlines$Passengers, frequency = 12, start = c(95)) # Converting the data into Time Series
View(airlines_ts)

airline_train <- airlines_ts[1:84] # Assigning the Whole data except last year data into airline_training Data
airline_test <- airlines_ts[85:96] # Assigning the last year dat into airline_testing data

airline_train <- ts(airline_train,frequency = 12) # Time Series of airline_training Data
airline_test <- ts(airline_test,frequency = 12) # Time Series of airline_testing Data

plot(airlines_ts) # Time Data Series Visualization

library(forecast)
library(smooth)

# Forecasting using Assigned Optimal Values

air_hw_a <- HoltWinters(airline_train, alpha = 0.2, beta = F, gamma = F) # With Alpha = 0.2
air_hwa_pred <- data.frame(predict(air_hw_a, n.ahead = 12)) # Prediction
plot(forecast(air_hw_a, h=12)) # Displays a horizontal Straight Line, doesn't show any seasonality and trend
air_hwa_mape <- MAPE(air_hwa_pred$fit, airline_test)*100 # Error = 17.2363


air_hw_ab <- HoltWinters(airline_train, alpha = 0.2, beta = 0.1, gamma = F) # Alpha and Beta as 0.2 and 0.1
air_hwab_pred <- data.frame(predict(air_hw_ab, n.ahead = 12)) # Prediction
plot(forecast(air_hw_ab, h=12)) # Displays a diagonal straight line, without any seasonality
air_hwab_mape <- MAPE(air_hwab_pred$fit, airline_test)*100 # Error = 11.55496


air_hw_abg <- HoltWinters(airline_train, alpha = 0.2, beta = 0.1, gamma = 0.1) # Alpha = 0.2, Beta = 0.1, Gamma = 0.1
air_hwabg_pred <- data.frame(predict(air_hw_abg, n.ahead = 12)) # Prediction
plot(forecast(air_hw_abg, h=12)) # Forecasts airline_test data with Level, Trend and Seasonality similar to airline_train data
air_hwabg_mape <- MAPE(air_hwabg_pred$fit, airline_test)*100 # Error = 6.5525


# Forecasting using Default Optimal Values 

air_hw_na <- HoltWinters(airline_train, beta = F, gamma = F) # With Alpha = True
air_hwna_pred <- data.frame(predict(air_hw_na, n.ahead = 12)) # Prediction
plot(forecast(air_hw_na, h = 12)) # Displays a horizontal Straight Line, doesn't show any seasonality and trend
air_hwna_mape <- MAPE(air_hwna_pred$fit, airline_test)*100 # Error = 18.55599

air_hw_nab <- HoltWinters(airline_train, gamma=F) # Alpha = True and Beta = True
air_hwnab_pred <- data.frame(predict(air_hw_nab, n.ahead = 12)) # Prediction
plot(forecast(air_hw_nab, h = 12)) # Displays a Diagonal Straight line, without any seasonality
air_hwnab_mape<-MAPE(air_hwnab_pred$fit, airline_test)*100 # Error = 13.10429

air_hw_nabg <- HoltWinters(airline_train) # Alpha = Beta = Gamma = True
air_hwnabg_pred <- data.frame(predict(air_hw_nabg, n.ahead = 12)) # Prediction
plot(forecast(air_hw_nabg, h = 12)) # Forecasts airline_test data with Level, Trend and Seasonality similar to airline_train data
air_hwnabg_mape <- MAPE(air_hwnabg_pred$fit, airline_test)*100 # Error = 1.730844


# Tabulating All the above Error values

air_df_mape <- data.frame(c("air_hwa_mape", "air_hwab_mape", "air_hwabg_mape", "air_hwna_mape", "air_hwnab_mape", "air_hwnabg_mape"), c(air_hwa_mape, air_hwab_mape, air_hwabg_mape, air_hwna_mape, air_hwnab_mape, air_hwnabg_mape))

colnames(air_df_mape) <- c("Air_MAPE", "Air_VALUES") # Renaming Column Names
View(air_df_mape)

# Creating a new model with very less Error

air_new_model <- HoltWinters(airlines_ts) # Built using default values of Alpha, Beta and Gamma and 100% of data
plot(forecast(air_new_model, n.ahead = 12)) # Plot of New Forecasted Model
air_forecast_new <- data.frame(predict(air_new_model, n.ahead = 12)) 
plot(predict(air_new_model, n.ahead = 12), main = "Forecasted Plot for next Year") # Plot of prediction of next year



# -- CocoCola Sales --

library(readxl)
cola_sales <- read_excel(choose.files())
View(cola_sales)
colnames(cola_sales) # Column Names

library(tseries)
cola_ts <- ts(cola_sales$Sales, frequency = 4, start = c(86)) # Converting the data into Time Series
View(cola_ts)

cola_train <- cola_ts[1:38] # Assigning the Whole data except last 4 quater data into Training Data
cola_test <- cola_ts[39:42] # Assigning the last 4 quater of the year data into Testing data

cola_train <- ts(cola_train,frequency = 4) # Time Series of Training Data
cola_test <- ts(cola_test,frequency = 4) # Time Series of Testing Data

plot(cola_ts) # Time Data Series Visualization

library(forecast)
library(smooth)

# Forecasting using Assigned Optimal Values

# Simple Exponential smoothing 
cola_ses_a <- ses(cola_train, alpha = 0.2) # With Alpha = 0.2
cola_sesa_pred <- data.frame(predict(cola_ses_a, h = 4)) # Prediction
plot(forecast(cola_ses_a, ahead = 4)) # Displays a horizontal Straight Line, doesn't show any seasonality and trend
cola_sesa_mape <- MAPE(cola_sesa_pred$Point.Forecast, cola_test)*100 # Error = 16.1243

# Holts winter method 
cola_holt_ab <- holt(cola_train, alpha = 0.2, beta = 0.1) # Alpha = 0.2 and Beta = 0.1
cola_holtab_pred <- data.frame(predict(cola_holt_ab, h = 4)) # Prediction
plot(forecast(cola_holt_ab, ahead = 4)) # Displays a diagonal straight line, without any seasonality
cola_holtab_mape <- MAPE(cola_holtab_pred$Point.Forecast, cola_test)*100 # Error = 8.498967

# Holts winter Exponential method
cola_hw_abg_new <- hw(cola_train, alpha = 0.2, beta = 0.1, gamma = 0.1) # Alpha = 0.2, Beta = 0.1 and Gamma = 0.1
cola_hwabg_pred_new <- data.frame(predict(cola_hw_abg_new, h = 4)) # Prediction
plot(forecast(cola_hw_abg_new, ahead = 4)) # Forecasts cola_test data with Level, Trend and Seasonality similar to cola_train data
cola_hwabg_mape_new <- MAPE(cola_hwabg_pred_new$Point.Forecast, cola_test)*100  # Error = 4.409854


# Forecasting using Default Optimal Values 

# Simple Exponential Method
cola_ses_na <- ses(cola_train, alpha = NULL) # With Beta = True and Gamma = True
cola_sesna_pred <- data.frame(predict(cola_ses_na, h = 4)) # Prediction
plot(forecast(cola_ses_na, ahead = 4)) # Displays a horizontal Straight Line, doesn't show any seasonality and trend
cola_sesna_mape <- MAPE(cola_sesna_pred$Point.Forecast, cola_test)*100 # Error = 9.132635

# Holts winter method 
cola_holt_nab <- holt(cola_train, alpha = NULL, beta = NULL) # Gamma = True
cola_holtnab_pred <- data.frame(predict(cola_holt_nab,h=4)) # Prediction
plot(forecast(cola_holt_nab, ahead=4)) # Displays a diagonal straight line, without any seasonality
cola_holtnab_mape <- MAPE(cola_holtnab_pred$Point.Forecast, cola_test)*100 # Error = 8.927388

# Holts winter Exponential method
cola_hw_nabg_new <- hw(cola_train, alpha = NULL, beta = NULL, gamma = NULL) # Alpha = Beta = Gamma = NULL
cola_hwnabg_pred_new <- data.frame(predict(cola_hw_nabg_new, h=4)) # Prediction
plot(forecast(cola_hw_nabg_new,ahead = 4)) # Forecasts cola_test data with Level, Trend and Seasonality similar to cola_train data
cola_hwnabg_mape_new <- MAPE(cola_hwnabg_pred_new$Point.Forecast, cola_test)*100 # Error = 3.826562

# Tabulating All the above Error values

cola_df_mapes_new <- data.frame(c("cola_sesa_mape", "cola_holtab_mape", "cola_hwabg_mape_new", "cola_sesna_mape", "cola_holtnab_mape", "cola_hwnabg_mape_new"), c(cola_sesa_mape, cola_holtab_mape, cola_hwabg_mape_new, cola_sesna_mape, cola_holtnab_mape ,cola_hwnabg_mape_new))
colnames(cola_df_mapes_new) <- c("Cola_MAPE", "Cola_VALUE")
View(cola_df_mapes_new)

# Creating a new model with very less Error

cola_new_model <- hw(cola_ts,alpha = NULL, beta = NULL, gamma = NULL) # Built using default values of Alpha, Beta and Gamma and 100% of data
plot(forecast(cola_new_model, ahead = 4)) # Plot of New Forecasted Model
cola_forecast_new <- data.frame(predict(cola_new_model, h = 4))
plot(predict(cola_new_model, n.ahead = 4), main = "Forecasted Plot for next Quater") # Plot of prediction of next quater



# -- Plastic Data --

plastic <- read.csv(choose.files()) # Importing Data
View(plastic)

plot(plastic$Sales, type = "o") # Data Visualization

months_input <- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 ) # Creating dummies for 12 months

colnames(months_input) <- month.abb # Assigning month names
plastic_input <- cbind(plastic, months_input) # Column Bind

plastic_input["t"] <- 1:60 # Creating New column

plastic_input["log_sales"] <- log(plastic_input["Sales"]) # Log of Sales column
plastic_input["t_square"] <- plastic_input["t"] * plastic_input["t"] # T Square column
View(plastic_input)
attach(plastic_input)

plastic_train <- plastic_input[1:48,] # Training Data
plastic_test <- plastic_input[49:60,] # Testing Data

# Model Building

linear_model<-lm(Sales ~ t, data = plastic_train) # Linear Model 
summary(linear_model) # Summary
linear_pred <- data.frame(predict(linear_model, interval = 'predict', newdata = plastic_test))
rmse_linear <- sqrt(mean((plastic_test$Sales - linear_pred$fit)^2, na.rm = T))
rmse_linear # RMSE =  260.9378

expo_model <- lm(log_sales ~ t, data = plastic_train) # Exponential  Model
summary(expo_model) # Summary
expo_pred <- data.frame(predict(expo_model, interval = 'predict', newdata = plastic_test))
rmse_expo <- sqrt(mean((plastic_test$Sales - exp(expo_pred$fit))^2, na.rm = T))
rmse_expo # RMSE =  268.6938

Quad_model <- lm(Sales ~ t + t_square, data = plastic_train) # Quadratic Model
summary(Quad_model) # Summary
Quad_pred <- data.frame(predict(Quad_model, interval = 'predict' ,newdata = plastic_test))
rmse_Quad <- sqrt(mean((plastic_test$Sales - Quad_pred$fit)^2, na.rm = T))
rmse_Quad # RMSE =  297.4067

# Additive Seasonality Model
sea_add_model <- lm(Sales ~ Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov ,data = plastic_train)
summary(sea_add_model) # Summary
sea_add_pred <- data.frame(predict(sea_add_model, interval = 'predict', newdata = plastic_test))
rmse_sea_add <- sqrt(mean((plastic_test$Sales - sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add # RMSE =  235.6027

# Additive Seasonality with Linear Model
Add_sea_Linear_model <- lm(Sales ~ t + Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov, data = plastic_train)
summary(Add_sea_Linear_model) # Summary
Add_sea_Linear_pred <- data.frame(predict(Add_sea_Linear_model, interval = 'predict', newdata = plastic_test))
rmse_Add_sea_Linear <- sqrt(mean((plastic_test$Sales - Add_sea_Linear_pred$fit)^2, na.rm = T))
rmse_Add_sea_Linear # RMSE =  135.5536

# Additive Seasonality with Quadratic
Add_sea_Quad_model <- lm(Sales ~ t + t_square + Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov, data = plastic_train)
summary(Add_sea_Quad_model) # Summary
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval = 'predict', newdata = plastic_test))
rmse_Add_sea_Quad <- sqrt(mean((plastic_test$Sales - Add_sea_Quad_pred$fit)^2, na.rm = T))
rmse_Add_sea_Quad # RMSE =  218.1939

# Multiplicative Seasonality
multi_sea_model <- lm(log_sales ~ Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov, data = plastic_train)
summary(multi_sea_model) # Summary
multi_sea_pred <- data.frame(predict(multi_sea_model, newdata=plastic_test, interval = 'predict'))
rmse_multi_sea <- sqrt(mean((plastic_test$Sales - exp(multi_sea_pred$fit))^2, na.rm = T))
rmse_multi_sea # RMSE =  239.6543

# Multiplicative Seasonality Linear trend
multi_add_sea_model <- lm(log_sales ~ t + Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov, data = plastic_train)
summary(multi_add_sea_model)  # Summary
multi_add_sea_pred <- data.frame(predict(multi_add_sea_model, newdata=plastic_test, interval = 'predict'))
rmse_multi_add_sea <- sqrt(mean((plastic_test$Sales - exp(multi_add_sea_pred$fit))^2, na.rm = T))
rmse_multi_add_sea # RMSE =  160.6833

# Preparing a table containing all RMSE Values 

table_rmse <- data.frame(c("rmse_linear", "rmse_expo", "rmse_Quad", "rmse_sea_add", "rmse_Add_sea_Quad", "rmse_multi_sea", "rmse_multi_add_sea"), c(rmse_linear, rmse_expo, rmse_Quad, rmse_sea_add, rmse_Add_sea_Quad, rmse_multi_sea, rmse_multi_add_sea))
colnames(table_rmse) <- c("model", "RMSE")
View(table_rmse)

# Multiplicative Seasonality Linear trend has least RMSE value

new_model <- lm(log_sales ~ t + Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov, data = plastic_input)
new_model_pred<-data.frame(predict(new_model, newdata = plastic_input, interval = 'predict'))
new_model_fin <- exp(new_model$fitted.values)
View(new_model_fin)

Month <- as.data.frame(plastic_input$Month)
Final <- as.data.frame(cbind(Month, plastic_input$Sales, new_model_fin))
colnames(Final) <-c("Month","Sales","New_Pred_Value")
plot(Final$Sales, main = "Actual Graph", xlab = "Sales(Actual)", ylab = "Months", col.axis = "blue", type = "o") 
plot(Final$New_Pred_Value, main = "Predicted Graph", xlab = "Sales(Predicted)", ylab = "Months", col.axis = "Green", type="s")
View(Final)

