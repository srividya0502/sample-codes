# Install necessary package if not already installed
# Install and load necessary packages
if (!require("readxl")) install.packages("readxl")
if (!require("tseries")) install.packages("tseries")
if (!require("forecast")) install.packages("forecast")
if (!require("vars")) install.packages("vars")
if (!require("ARDL")) install.packages("ARDL")
if (!require("dynlm")) install.packages("dynlm")
if (!require("strucchange")) install.packages("strucchange")

library(readxl)
library(tseries)
library(forecast)
library(vars)
library(ARDL)
library(dynlm)
library(urca)
library(strucchange)

# Loading the dataset into the R console
data <- read_excel("E:/MSE/3rd sem/AMFE/model1-dataset-Int 2.xlsx")
# Renaming columns
colnames(data)[which(colnames(data) == "federal funds rate")] <- "federal_funds_rate"
colnames(data)[which(colnames(data) == "exports/imports")] <- "exports_by_imports"

# Converting to type timeseries for ease in data handling on R
ts_data <- ts(data[, c("REER", "GDP_growth", "exports_by_imports", "federal_funds_rate", "Remittance")], start=c(2000, 1), frequency=4)

# Converting individual variables to type ts
Remittance <- ts(data$Remittance, start=c(2000, 1), frequency=4) 
GDP_growth <- ts(data$GDP_growth, start=c(2000, 1), frequency=4)
exports_by_imports <- ts(data$exports_by_imports, start=c(2000, 1), frequency=4)
federal_funds_rate <- ts(data$federal_funds_rate, start=c(2000, 1), frequency=4)
REER <- ts(data$REER, start=c(2000, 1), frequency=4)

# Conducting ADF tests to test for stationarity 
adf_reer <- adf.test(data$REER)
adf_remittance <- adf.test(data$Remittance)
adf_exports_imports <- adf.test(data$exports_by_imports)
adf_federal_funds_rate <- adf.test(data$federal_funds_rate)
adf_gdp<- adf.test(data$GDP_growth)

# Printing the test results
list(ADF_REER = adf_reer, ADF_Remittance = adf_remittance, 
     ADF_Exports_Imports = adf_exports_imports,
     ADF_Federal_Funds_Rate = adf_federal_funds_rate,
     ADF_GDP = adf_gdp)

# First differencing of non-stationary variables
data_diff <- data.frame(
  Remittance_diff = na.omit(diff(data$Remittance)),
  Exports_imports_diff = na.omit(diff(data$exports_by_imports)),
  Federal_funds_rate_diff = na.omit(diff(data$federal_funds_rate)))

# Performing ADF test for differenced variables, handling NAs using na.omit
adf_diff_remittance <- adf.test(data_diff$Remittance_diff)
adf_diff_exports_imports <- adf.test(data_diff$Exports_imports_diff)
adf_diff_federal_funds_rate <- adf.test(data_diff$Federal_funds_rate_diff)

#Printing the test results
list(ADf_Remittance = adf_diff_remittance, 
     ADf_Exports_Imports = adf_diff_exports_imports,
     ADf_Federal_Funds_Rate = adf_diff_federal_funds_rate)

# Checking lag for the model as a whole
lag_selection <- VARselect(ts_data, lag.max=10, type="const")
print(lag_selection)

# Lag selection for all variables individually
lag_remittance <- VARselect(Remittance, lag.max=10, type="const")
lag_gdp_growth <- VARselect(GDP_growth, lag.max=10, type="const")
lag_exports_by_imports <- VARselect(exports_by_imports, lag.max=10, type="const")
lag_federal_funds_rate <- VARselect(federal_funds_rate, lag.max=10, type="const") 
lag_reer <- VARselect(REER, lag.max=10, type="const")

# Printing the best lag results
print(lag_remittance)
print(lag_gdp_growth)
print(lag_exports_by_imports)
print(lag_federal_funds_rate)
print(lag_reer)

#Running the ARDL Model because variables are a mix of I(0) and I(1) variables
model <- ardl(REER ~ GDP_growth + exports_by_imports + federal_funds_rate + Remittance, data=ts_data, order=c(1, 3, 4, 2, 3)) 
summary(model)

#Running for ARDL cointegration
bounds_test <- bounds_f_test(model, case = 3)
print(bounds_test)

#RESULT- No cointegration

# Run Auto ARDL model-trial 1 to check if auto model gives better lags
auto_ardl_model1 <- auto_ardl(REER ~ GDP_growth + exports_by_imports + federal_funds_rate + Remittance, 
                             data = ts_data, max_order = c(1, 5, 5, 5, 5))

# Summary of the Auto ARDL model-trial 1
summary(auto_ardl_model1)
auto_ardl_model1$top_orders

# Fitting Auto ARDL model-trial 2
auto_ardl_model2 <- auto_ardl(REER ~ GDP_growth + exports_by_imports + federal_funds_rate + Remittance, 
                              data = ts_data, max_order = c(1, 10, 10, 10, 10))

# Summary of the Auto ARDL model-trial 2
summary(auto_ardl_model2)
auto_ardl_model2$top_orders

#None of the lag orders given by the auto_ardl function are backed with good economic intuition. 
#Selecting the first model (model) as best model

#Checking the residuals using CUSUM test
# Extracting residuals from the ARDL model
residuals_ardl <- residuals(model)
# Creating a data frame with the residuals
cusum_data <- data.frame(time = 1:length(residuals_ardl), residuals = residuals_ardl)
# Performing the CUSUM test on the residuals
cusum_test <- efp(residuals ~ time, data = cusum_data, type = "Rec-CUSUM")
# Plotting the CUSUM test results
plot(cusum_test)

#Residual check for the selected model
bg_test <- bgtest(model)
print(bg_test)
