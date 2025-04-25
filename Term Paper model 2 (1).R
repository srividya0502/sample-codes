# Install and load necessary packages
if (!require("readxl")) install.packages("readxl")
if (!require("tseries")) install.packages("tseries")
if (!require("forecast")) install.packages("forecast")
if (!require("vars")) install.packages("vars")
if (!require("ARDL")) install.packages("ARDL")
if (!require("dynlm")) install.packages("dynlm")
if (!require("urca")) install.packages("urca")

library(readxl)
library(tseries)
library(forecast)
library(vars)
library(ARDL)
library(dynlm)
library(urca)

# Loading the dataset into R console
data <- read_excel("E:/MSE/3rd sem/AMFE/Model2-dataset-int2.xlsx")
# Renaming columns
colnames(data)[which(colnames(data) == "federal funds rate")] <- "federal_funds_rate"
colnames(data)[which(colnames(data) == "exports/imports")] <- "exports_by_imports"

# Conducting ADF to test for stationarity
adf_results <- list(
  ADF_TNT = adf.test(data$tradables_non_tradables),
  ADF_Remittance = adf.test(data$Remittance),
  ADF_Exports_Imports = adf.test(data$exports_by_imports),
  ADF_Federal_Funds_Rate = adf.test(data$federal_funds_rate),
  ADF_GDP = adf.test(data$GDP_growth)
)
print(adf_results)

# Defining the variables for which differencing is required
# Creating the differenced data frame
data_diff <- data.frame(
  Tnt_diff = diff(data$tradables_non_tradables),
  GDP_growth_diff = diff(data$GDP_growth),
  Exports_imports_diff = diff(data$exports_by_imports),
  Federal_funds_rate_diff = diff(data$federal_funds_rate),
  Remittance_diff = diff(data$Remittance)
)

# Running ADF tests on the differenced variables
adf_diff_remittance <- adf.test(data_diff$Remittance_diff)
adf_diff_exports_imports <- adf.test(data_diff$Exports_imports_diff)
adf_diff_tnt <- adf.test(data_diff$Tnt_diff)
adf_diff_federal_funds_rate <- adf.test(data_diff$Federal_funds_rate_diff)

# Printing the test results
list(
  ADF_Diff_Remittance = adf_diff_remittance, 
  ADF_Diff_Exports_Imports = adf_diff_exports_imports,
  ADF_Diff_Tnt = adf_diff_tnt,
  ADF_Diff_Federal_Funds_Rate = adf_diff_federal_funds_rate
)

# Converting to time series format
ts_data <- ts(data[, c("tradables_non_tradables", "GDP_growth", "exports_by_imports", "federal_funds_rate", "Remittance")], start=c(2000, 1), frequency=4)
ts_data <- as.data.frame(ts_data)
colnames(ts_data) <- c("tradables_non_tradables", "GDP_growth", "exports_by_imports", "federal_funds_rate", "Remittance")

ts_remittance <- ts(data$Remittance, start=c(2000, 1), frequency=4) 
ts_gdp_growth <- ts(data$GDP_growth, start=c(2000, 1), frequency=4)
ts_exports_by_imports <- ts(data$exports_by_imports, start=c(2000, 1), frequency=4)
ts_federal_funds_rate <- ts(data$federal_funds_rate, start=c(2000, 1), frequency=4)
ts_tnt <- ts(data$tradables_non_tradables, start=c(2000, 1), frequency=4)

#Finding the information criterion values of all 10 lags(parsimony) for each variable
lag_selection <- VARselect(ts_data, lag.max = 10, type = "const")
print(lag_selection$selection)
lag_remittance <- VARselect(ts_remittance, lag.max=10, type="const")
lag_gdp_growth <- VARselect(ts_gdp_growth, lag.max=10, type="const")
lag_exports_by_imports <- VARselect(ts_exports_by_imports, lag.max=10, type="const")
lag_federal_funds_rate <- VARselect(ts_federal_funds_rate, lag.max=10, type="const") 
lag_tnt <- VARselect(ts_tnt, lag.max=10, type="const")

# Printing the results
print(lag_remittance)
print(lag_gdp_growth)
print(lag_exports_by_imports)
print(lag_federal_funds_rate)
print(lag_tnt)

# Fitting ARDL model with the best lags found according to BIC
model_0 <- ardl(tradables_non_tradables ~ GDP_growth + exports_by_imports + federal_funds_rate + Remittance, data=ts_data, order=c(5, 3, 4, 2, 5)) 
summary(model)

#Performing Bounds test for Cointegration
bounds_test <- bounds_f_test(model, case = 3)
print(bounds_test)
#Bounds test indicates a possible Cointegration. Therefore the ECM has to be checked after finding the optimal ARDL models.

#Finding R suggested optimal lags with three different max orders
auto_ardl_model_1 <- auto_ardl(tradables_non_tradables ~ GDP_growth + exports_by_imports + federal_funds_rate + Remittance, 
                               data = ts_data, max_order = c(10, 10, 10, 10, 10), selection = "BIC")
auto_ardl_model_2 <- auto_ardl(tradables_non_tradables ~ GDP_growth + exports_by_imports + federal_funds_rate + Remittance, 
                               data = ts_data, max_order = c(5, 5, 5, 5, 5), selection = "BIC")
auto_ardl_model_3 <- auto_ardl(tradables_non_tradables ~ GDP_growth + exports_by_imports + federal_funds_rate + Remittance, 
                               data = ts_data, max_order = c(1, 5, 5, 5, 5), selection = "BIC")

# Summary of the Auto ARDL models
summary(auto_ardl_model_1)
auto_ardl_model_1$top_orders
summary(auto_ardl_model_2)
auto_ardl_model_2$top_orders
summary(auto_ardl_model_3)
auto_ardl_model_3$top_orders

#Fitting trial ARDL model
trial_model_1<- ardl(tradables_non_tradables ~ GDP_growth + exports_by_imports + federal_funds_rate + Remittance, 
                     data = ts_data, order = c(5, 4, 0, 5, 0))#optimal lag for max order(10,10,10,10,10)
summary(trial_model_1)
trial_model_2<- ardl(tradables_non_tradables ~ GDP_growth + exports_by_imports + federal_funds_rate + Remittance, 
                     data = ts_data, order = c(5, 4, 0, 5, 1))#optimal lag for max order(10,10,10,10,10) and lag 1 for remittances
summary(trial_model_2)
trial_model_3<- ardl(tradables_non_tradables ~ GDP_growth + exports_by_imports + federal_funds_rate + Remittance, 
                     data = ts_data, order = c(4, 3, 4, 0, 1))#optimal lag for max order(5,5,5,5,5) and lag 1 for remittances
summary(trial_model_3)
trial_model_4<- ardl(tradables_non_tradables ~ GDP_growth + exports_by_imports + federal_funds_rate + Remittance, 
                     data = ts_data, order = c(1, 0, 1, 0, 1))#optimal lag for max order(1,5,5,5,5) and lag 1 for remittances
summary(trial_model_4)
trial_model_5<- ardl(tradables_non_tradables ~ GDP_growth + exports_by_imports + federal_funds_rate + Remittance, 
                     data = ts_data, order = c(1, 0, 1, 0, 4))#optimal lag for max order(1,5,5,5,5) and lag 4 for remittances
summary(trial_model_5)
#we choose trial_model_5 as the best fit model.

# Prepare ECM based on the long-run relationship from the ARDL model

# Fitting the long-run model to obtain the ECT
long_run_model <- lm(tradables_non_tradables ~ GDP_growth + exports_by_imports + federal_funds_rate + Remittance, data = ts_data)
ts_data <- as.data.frame(ts_data)  
ts_data$ECT <- residuals(long_run_model)

# Creating differenced variables for ECM
ts_data$diff_tradables_non_tradables <- c(NA, diff(ts_data$tradables_non_tradables))
ts_data$diff_GDP_growth <- c(NA, diff(ts_data$GDP_growth))
ts_data$diff_exports_by_imports <- c(NA, diff(ts_data$exports_by_imports))
ts_data$diff_federal_funds_rate <- c(NA, diff(ts_data$federal_funds_rate))
ts_data$diff_Remittance <- c(NA, diff(ts_data$Remittance))

# Fitting the ECM model based on the optimal lags (1,0,1,0,4) from the best fit model
ecm_model <- dynlm(diff_tradables_non_tradables ~ 
                     L(diff_tradables_non_tradables, 1) +
                     diff_GDP_growth + L(diff_GDP_growth, 1) +
                     diff_exports_by_imports + L(diff_exports_by_imports, 1) +
                     diff_federal_funds_rate + L(diff_federal_funds_rate, 1) +
                     diff_Remittance + L(diff_Remittance, 1) + L(diff_Remittance, 2) + L(diff_Remittance, 3) + L(diff_Remittance, 4) +
                     L(ECT, 1), data = ts_data)

# Summary of ECM model
summary(ecm_model)

#Checking the residuals using CUSUM test
# Extracting residuals from the ARDL model
residuals_ardl <- residuals(model)
# Creating a data frame with the residuals
cusum_data <- data.frame(time = 1:length(residuals_ardl), residuals = residuals_ardl)
# Performing the CUSUM test on the residuals
cusum_test <- efp(residuals ~ time, data = cusum_data, type = "Rec-CUSUM")
# Plotting the CUSUM test results
plot(cusum_test)

#Residual check
bg_test <- bgtest(trial_model_5)
print(bg_test)
