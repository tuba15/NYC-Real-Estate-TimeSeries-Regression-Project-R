# install necessary packages for TASK1,2,3 
install.packages("ggplot2") 
install.packages("scales") 
install.packages("odbc") 
install.packages("DBI")  
install.packages("tidyverse")  # TO GET ALL YEARS SO THAT IT CAN FILTER FROM 2009 YEAR
install.packages("lubridate") #THIS PACKAGE FOR MAKING NEW COLOMN USING MUTATE FUNCTION()
install.packages("forecast")  #THIS PACKAGE IS FOR TIME SERIES FORECASTING where ts( ) function is used 
library(odbc) 
library(DBI) 
library(tidyverse) 
library(lubridate) 
library(forecast) 
library(ggplot2) 
library(scales) 
install.packages("car") 
library(car) 
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# TASK 1 TIME SERIES FORECASTING OF TOTAL SALES FROM 2009
.............................................................................................................................................................................................................................................
#IMPORTING CSV FILES 
borough_id <- read_csv(file.choose()) 
building_class <- read_csv(file.choose()) 
neighborhood_name <- read_csv(file.choose()) 
nyc_data <- read_csv(file.choose()) 
...........................................................................................................................................................................................................................................................
#JOINING DATA INTO 1 
All_Joined1<- nyc_data %>%
  left_join(building_class, by = c("BUILDING_CLASS_FINAL_ROLL" = "BUILDING_CODE_ID")) %>%
  left_join(neighborhood_name, by = c("NEIGHBORHOOD_ID" = "NEIGHBORHOOD_ID")) 
....................................................................................................................................................................................................  
# CODE TO CALCULATE TOTAL SALES SO USED SUM TO ADD TOTAL SALE PRICE FIRST THEN ALSO MADE QUARTER COLOMN
  quarterly_sales <- All_Joined1 %>%
      filter(NEIGHBORHOOD_ID == "81", 
                        SALE_PRICE > 1, 
                        year(SALE_DATE) >= 2009) %>%
                        mutate(               #creating a new colomn with mutate function
                        Year = year(SALE_DATE),
                        Quarter = quarter(SALE_DATE) # quarters are divided through sale_date 
           ) %>%
                    group_by(Year, Quarter) %>%
 summarise(
 TotalSales = sum(SALE_PRICE, na.rm = TRUE)
           ) %>%
        ungroup() %>%
   arrange(Year, Quarter)  # arrange () to arrange all the years accordingly in chrononical order

View(quarterly_sales) 
.................................................................................................................................................................................................................
#NOW CONVERTING data to time series object 
time_seriesdata <- ts( #ts function is used to  convert data into time series object 
  quarterly_sales$TotalSales,
  start=c(2009,1) ,#c(2009,1) means from year 2009 1 which is jan-march
  frequency = 4,
  names = "TotalSales"
) 
str(quarterly_sales$TotalSales) 
options(scipen = 999) # scipen 999 for avoiding standard notation

print(time_seriesdata) 

#sumarising the time series for better interpretation
summary(time_seriesdata) 
#printing 
print(
  data.frame(
    Year_Quarter = paste(floor(time(time_seriesdata)), "Q", cycle(time_seriesdata)),
    Sales = scales::dollar(as.numeric(time_seriesdata)) # to add dollar sign for sale prices scale::dollar is used and to make sure its in numerical data it is as.numeric used
  ),
  row.names = FALSE
) 
..............................................................................................................................................................................................................................
# USING ARIMA FOR FORECATING MODEL SO auto.arima function used 
use_arima <- auto.arima(time_seriesdata)  #arima is a model to predict the future sales in Elmhurst neighborhood 

summary(use_arima)   
print(use_arima) 
.............................................................................................................................................................................................................................................
#8 QUARTERS FORECASTING 
forecast_8q <- forecast(use_arima,h=8) 
print(forecast_8q)  # View point forecasts and confidence intervals
summary(forecast_8q) 
......................................................................................................................................................................................................................
#VISUALIZATING THE FORECASTING USING AUTOPLOT 
autoplot(forecast_8q) + # AUTOPLOT IS A FUNCTION TO PLOT ARIMA MODEL AS GRAPH
  labs(
    title = "Forecast of residential sales in Elmhurst neighborhood for 8 quarter",
    x = "Year", 
    y = "Total Sales ($)"
  )  

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#TASK 2 MULTIPLE LINEAR REGRESSION 
  #ANOTHER FORECAST FOR MULTIPLE LINEAR REGRESSION INCLUDING TIME AND SEASONALITY FROM 2009
  
regression_data <- quarterly_sales %>%
  filter(SALE_YEAR >= 2009) %>% 
    mutate(
    Time = 1:n(),  # Sequential time period
    Q1 = as.numeric(Quarter == 1),# this is done for converting numerical year date to quarter like eg 2009 Q1
    Q2 = as.numeric(Quarter == 2),
    Q3 = as.numeric(Quarter == 3), 
    # Q4 is the reference category
    Year = 2009 + (Time - 1) %/% 4,
    QuarterLabel = paste0("Q", Quarter),
    DateLabel = paste(Year, QuarterLabel)  # e.g., "2009 Q1"
  ) 

# View the prepared data
print(regression_data, n = Inf) 


# multiple regression model with time trend and seasonality
reg_model <- lm(TotalSales ~ Time + Q1 + Q2 + Q3, data = regression_data) #lm()  USED TO MAKE MULTIPLE REGRESSION MODEL

# Model summary
summary(reg_model) 

# Create future time periods for forecasting
future_periods <- data.frame( 
  Time = (nrow(regression_data)+1):(nrow(regression_data)+8), #_nrow(regression_Data) means last inndex of historical data which creates a sequence (last index+1),(last index+8) like If historical data ends at Time=60 → 61,62,...,68
  Quarter = rep(1:4, 2)[1:8] #repeating the quarters1-quarter 4 like quarter 1,2,3,4,1,2,3,4 for 2 times 
) %>%
  mutate(
    Q1 = as.numeric(Quarter == 1),#this will create reates binary indicator (1/0) for Q1   #Quarter == 1 returns TRUE/FALSE → as.numeric() converts to 1/0 #Example: For Quarter=1 → Q1=1; Quarter=2 → Q1=0
    Q2 = as.numeric(Quarter == 2),
    Q3 = as.numeric(Quarter == 3)
  ) 
print(future_periods) 
#generatepredictions
reg_forecast <- predict(reg_model, newdata = future_periods, interval = "prediction") # in this with reg_model used predicting total sales  for future also calculating confidence interval
reg_results <- cbind(future_periods, reg_forecast) #combination of future predicted and predicted values into one data

#  ETS Model
ets_model <- ets(time_seriesdata) # this is for doing ets model,e-erroe,t-time,s-seasonality to time_seriesdata 
ets_forecast <- forecast(ets_model, h = 8) # This function takes a time series model and projects future values.h = 8: This in to forecast 8 future time periods (e.g., 8 quarter
print(reg_results) 
print(ets_forecast) 
print(ets_model) 

#  Combined Plot Data # for combining historical  data AND FUTURE DATA
combined_data <- data.frame(
  Time = c(regression_data$Time, future_periods$Time),
  Quarter = c(regression_data$Quarter, future_periods$Quarter),#Combines the Quarter names or labels (e.g., "Q1 2023", "Q2 2023", etc.) from past and future data.

  Actual = c(regression_data$TotalSales, rep(NA, 8)), #actual sales values from your regression data, and for the future 8 period
  Forecast = c(rep(NA, nrow(regression_data)), reg_results$fit),
  Lower = c(rep(NA, nrow(regression_data)), reg_results$lwr),# LOWER BOUND OF CONFIDENCE INTERVAL FOR REGRESSION
  Upper = c(rep(NA, nrow(regression_data)), reg_results$upr)# UPPER BOND FOR C.I FOR REGRESSION
) 

#.REGRESSION FORECAST PLOTIING 
ggplot(combined_data, aes(x = Time)) +
  geom_line(aes(y = Actual), color = "plum", linewidth = 1) +
  geom_line(aes(y = Forecast), color = "red", linetype = "dashed", linewidth = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +#THIS FOR ADDING SHADED LINE FOR CONFIDENCE INTERVAL
  labs(title = "Regression Forecast with Seasonality",
       subtitle = "Actual vs Predicted Sales with 95% Confidence Interval",
       x = "Time Period", 
       y = "Total Sales ($)") +
  scale_y_continuous(labels = dollar_format()) + 
  theme_minimal() 

#ets forceast plot 
autoplot(ets_forecast) + #AUTOPLOT-USED FOR VISUALZIATION FOR TIME SERIES 
  labs(title = "ETS Model Forecast",
       x = "Year", 
       y = "Total Sales ($)") +
  scale_y_continuous(labels = dollar_format())  

# Calculate accuracy metrics
reg_accuracy <- accuracy(predict(reg_model), regression_data$TotalSales) #predict(reg_model) gives the predicted sales from your regression model.regression_data$TotalSales is the actual sales.accuracy() compares the two and calculates error metrics like: ME: Mean ErrorRMSE: Root Mean Squared ErrorMAE: Mean Absolute ErrorMAPE: Mean Absolute Percentage ErrorMPE: Mean Percentage ErrorMASE: Mean Absolute Scaled Error
ets_accuracy <- accuracy(ets_model) 

cat("Regression Accuracy:\n"); print(reg_accuracy) 
cat("\nETS Accuracy:\n"); print(ets_accuracy) 
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #TASK 2 PART 2
  # IN ELMHURST NEIGHBORHOOD CREATING MULTIPLE REGRESSION MODEL FOR RESIDENTIAL SALES WITH SALE DATE,YEAR BUILT,BUILDING TYPE.GROSS SQAURE UNIT,NO OF UNITS
  # Step 1: Cleaning  filter for purely residential properties
 # Load libraries
  # Load libraries
  library(dplyr)
library(ggplot2)
library(lubridate)
library(car)
library(scales)

# 1. Prepare data: Residential-only properties in Elmhurst
model_data <- All_Joined1 %>% 
  filter(
    NEIGHBORHOOD_NAME == "ELMHURST",
    SALE_PRICE > 10000,
    RESIDENTIAL_UNITS > 0,
    COMMERCIAL_UNITS == 0,
    !is.na(GROSS_SQUARE_FEET),
    !is.na(YEAR_BUILT),
    !is.na(SALE_DATE)
  ) %>%
  mutate(
    SALE_DATE = as.Date(SALE_DATE),
    PROPERTY_AGE = year(SALE_DATE) - YEAR_BUILT,
    TOTAL_UNITS = RESIDENTIAL_UNITS,
    BUILDING_TYPE = factor(BUILDING_CLASS_FINAL_ROLL),
    LOG_SALE_PRICE = log(SALE_PRICE)
  ) %>%
  select(LOG_SALE_PRICE, PROPERTY_AGE, GROSS_SQUARE_FEET, TOTAL_UNITS, BUILDING_TYPE) %>%
  na.omit() 
print(model_data) 

# 2. Build regression model
model_data<-lm(
  LOG_SALE_PRICE ~ PROPERTY_AGE + GROSS_SQUARE_FEET + TOTAL_UNITS + BUILDING_TYPE,SALE_DATE,
  data = model_data
) 
summary(model_residential) 

# 3. Most/least useful predictors (manual summary extract)
model_summary <- summary(model_residential) 
coefficients_df <- data.frame(
  term = rownames(model_summary$coefficients),
  estimate = model_summary$coefficients[, "Estimate"],
  p_value = model_summary$coefficients[, "Pr(>|t|)"]
) 

# 4. Visualize predictor usefulness
ggplot(coefficients_df[coefficients_df$term != "(Intercept)", ], 
       aes(x = reorder(term, estimate), y = estimate, fill = p_value < 0.05)) +
  geom_col() +
  coord_flip() +
  labs(title = "Effect of Predictors on Log(Sale Price)",
       x = "Predictor",
       y = "Coefficient Estimate") +
  scale_fill_manual(values = c("gray", "steelblue"), labels = c("Not Significant", "Significant")) +
  theme_minimal() 

# 5. Check redundancy using VIF
vif_values <- vif(model_residential) 
print(vif_values) 

# 6. Add prediction and residuals
model_data$PREDICTED_LOG <- predict(model_residential) 
model_data$PREDICTED_PRICE <- exp(model_data$PREDICTED_LOG) 
model_data$ACTUAL_PRICE <- exp(model_data$LOG_SALE_PRICE) 
model_data$residuals <- model_data$ACTUAL_PRICE - model_data$PREDICTED_PRICE 

# 7. Visualization bargain vs overpriced 
ggplot(model_data, aes(x = ACTUAL_PRICE, y = PREDICTED_PRICE)) +
  geom_point(aes(color = residuals), size = 2, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_color_gradient2(low = "red", mid = "skyblue",high ="purple",midpoint =0)+
  labs(title = "Bargains vs Overpriced Properties",
       x = "Actual Sale Price",
       y = "Predicted Sale Price",
       color = "Residual (Error)") +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = dollar) +
  theme_minimal() 

# 8. Print top 5 bargains and overpriced properties
cat("\nTop 5 Bargains:\n")
print(head(model_data %>% arrange(desc(residuals)) %>% 
             select(ACTUAL_PRICE, PREDICTED_PRICE, residuals), 5)) 

cat("\nTop 5 Overpriced:\n")
print(head(model_data %>% arrange(residuals) %>% 
             select(ACTUAL_PRICE, PREDICTED_PRICE, residuals), 5)) 

# FILTER & CREATE QUARTERLY SALES STARTING FROM 2013
quarterly_sales <- All_Joined1 %>%
  filter(NEIGHBORHOOD_ID == "81", SALE_PRICE > 1, year(SALE_DATE) >= 2013) %>%
  mutate(
    SALE_DATE = as.Date(SALE_DATE),
    Year = year(SALE_DATE),
    Quarter = quarter(SALE_DATE)
  ) %>%
  group_by(Year, Quarter) %>%
  summarise(TotalSales = sum(SALE_PRICE, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Year, Quarter) 

# FILL MISSING QUARTERS FROM 2013 ONWARD
all_quarters <- expand.grid(
  Year = 2013:max(quarterly_sales$Year),
  Quarter = 1:4
)
quarterly_sales_full <- all_quarters %>%
  left_join(quarterly_sales, by = c("Year", "Quarter")) %>%
  mutate(TotalSales = ifelse(is.na(TotalSales), 0, TotalSales)) %>%
  arrange(Year, Quarter)

# CREATE TIME SERIES OBJECT STARTING FROM 2013 Q1
time_seriesdata <- ts(
  quarterly_sales_full$TotalSales,
  start = c(2013, 1),
  frequency = 4
)

# PLOT & FORECAST
plot(time_seriesdata, main = "Quarterly Sales in Elmhurst (2013 Onward)", ylab = "Total Sales", xlab = "Time")

# APPLY ETS MODEL (M,A,M)
ets_model <- ets(time_seriesdata, model = "MAM")
summary(ets_model)

# FORECAST 8 FUTURE QUARTERS
forecast_8q <- forecast(ets_model, h = 8)
print(forecast_8q)

# PLOT FORECAST
autoplot(forecast_8q) +
  ggtitle("ETS Forecast of Elmhurst Sales (Next 8 Quarters from 2025)") +
  xlab("Year") + ylab("Total Sales")

