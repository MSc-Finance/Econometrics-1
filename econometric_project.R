# Install necessary packages
# install.packages("readxl")
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("tseries")
# install.packages("gridExtra")
# install.packages("xtable")
# install.packages("urca")
# install.packages("seasonal")
#install.packages("forecast")
#install.packages("dplyr")
#install.packages("tidyr")
# install.packages("vars")
# install.packages("BVAR")
# install.packages("gridGraphics")

# Load the packages
library(readxl)
library(ggplot2)
library(lubridate)
library(tseries)
library(gridExtra)
library(urca)
library(seasonal)
library(zoo)
library(forecast)
library(dplyr)
library(tidyr)
library(BVAR)
library(vars)
library(gridGraphics)

# Replace 'path/to/your/directory' with the actual path
setwd('C:/Users/rvs88/iCloudDrive/Work/Lambda/6 Asesorías Lambda/Emely Torres/')

# Read the data from Excel
data <- read_excel("data/data_peru.xlsx")

# Check if dates are already in Date format or convert them
if(!inherits(data$dates, "Date")) {
  data$dates <- as.Date(data$dates)
}


## QUESTION 1

# Using ggplot2 for a 2x2 grid of plots
# EPI plot
p1 <- ggplot(data, aes(x = dates, y = epi)) +
  geom_line() +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_minimal() +
  labs(title = "Export Price index", x = "Date", y = "EPI")

# GDP plot
p2 <- ggplot(data, aes(x = dates, y = gdp)) +
  geom_line() +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_minimal() +
  labs(title = "Real GDP", x = "Date", y = "GDP")

# Tax plot
p3 <- ggplot(data, aes(x = dates, y = tax)) +
  geom_line() +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_minimal() +
  labs(title = "Tax Revenues", x = "Date", y = "Tax")

# ER plot
p4 <- ggplot(data, aes(x = dates, y = er)) +
  geom_line() +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_minimal() +
  labs(title = "Nominal Exchange Rate", x = "Date", y = "ER")

# Arrange the plots in a 2x2 grid
library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)# Using ggplot2 for a 2x2 grid of plots



# ===============================================
# --------- GDP Stationarity Analysis -----------
# ===============================================

# Assuming your data has a date column in 'yyyy-mm-dd' format
data$year <- as.numeric(format(as.Date(data$dates), "%Y"))
data$quarter <- as.numeric(format(as.Date(data$dates), "%m")) %/% 4 + 1

# Convert GDP and TAX to time series
gdp_ts <- ts(data$gdp, start = c(min(data$year), min(data$quarter)), frequency = 4)
tax_ts <- ts(data$tax, start = c(min(data$year), min(data$quarter)), frequency = 4)

# Seasonal Adjustment for GDP and TAX
gdp_sa <- seas(x = gdp_ts, x11 = "")
tax_sa <- seas(x = tax_ts, x11 = "")
data$gdp_sa <- as.numeric(final(gdp_sa))
data$tax_sa <- as.numeric(final(tax_sa))

# Real GDP Seasonally Adjusted plot
gdp_plot <- ggplot(data, aes(x = dates, y = gdp_sa)) +
  geom_line(size = 0.6) +  # Increase line width
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_minimal() +
  labs(title = "Peru: Real GDP Seasonally Adj.", x = "Date", y = "GDP")

# GDP plot with linear trend (without interval, blue dashed line)
gdp_trend_plot <- ggplot(data, aes(x = dates, y = gdp_sa)) +
  geom_line(size = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "dashed", size = 0.7) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_minimal() +
  labs(title = "Real GDP with Linear Trend Line", x = "Date", y = "GDP")

# GDP plot with 4th degree polynomial trend (without interval, blue dashed line)
gdp_poly_plot <- ggplot(data, aes(x = dates, y = gdp_sa)) +
  geom_line(size = 0.6) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), se = FALSE, color = "red", linetype = "dashed", size = 0.7) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_minimal() +
  labs(title = "Real GDP with Polynomial Trend", x = "Date", y = "GDP")

# Arrange the plots side by side
grid.arrange(gdp_plot, gdp_trend_plot, gdp_poly_plot, ncol = 3)


# ===============================================
# -------------- Unit Root Tests ----------------
# ===============================================

# Define the variables to test
variables <- c("epi", "gdp_sa", "tax_sa", "er")

# Initialize a list to store results
adf_results_list <- list()

# Loop over variables and perform ADF tests
for (variable in variables) {
  series <- data[[variable]]
  
  # Perform ADF test
  test_result <- adf.test(series, alternative = "stationary")
  
  # Store results
  adf_results_list[[variable]] <- data.frame(
    Variable = variable,
    Test_Statistic = test_result$statistic,
    P_Value = test_result$p.value
  )
}

# Combine results from all variables into a single data frame
adf_results <- do.call(rbind, adf_results_list)
adf_results


# ===============================================
# ---------- Variable transformation ------------
# ===============================================

# Convert 'dates' to Date format if it's not already
data$dates <- as.Date(data$dates)

# Sort data by dates if not already sorted
data <- data[order(data$dates), ]

# Calculate YoY growth rates for quarterly data
# Shift the 'epi' values by 4 (one year) and calculate the growth rate
data$epi_growth <- ((data$epi / dplyr::lag(data$epi, 4) - 1) * 100)
data$gdp_growth <- (data$gdp_sa / dplyr::lag(data$gdp_sa, 4) - 1) * 100
data$er_growth <- (data$er / dplyr::lag(data$er, 4) - 1) * 100

# Adjust tax_sa for inflation
data$tax_sa_real <- data$tax_sa / data$cpi
data$tax_growth <- (data$tax_sa_real / dplyr::lag(data$tax_sa_real, 4) - 1) * 100


# EPI plot
p1 <- ggplot(data, aes(x = dates, y = epi_growth)) +
  geom_line() +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_minimal() +
  labs(title = "Export Price index Growth", x = "Date", y = "EPI")

# GDP plot
p2 <- ggplot(data, aes(x = dates, y = gdp_growth)) +
  geom_line() +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_minimal() +
  labs(title = "Real GDP Growth", x = "Date", y = "GDP")

# Tax plot
p3 <- ggplot(data, aes(x = dates, y = tax_growth)) +
  geom_line() +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_minimal() +
  labs(title = "Tax Revenues Growth", x = "Date", y = "Tax")

# ER plot
p4 <- ggplot(data, aes(x = dates, y = er_growth)) +
  geom_line() +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_minimal() +
  labs(title = "Exchange Rate Growth", x = "Date", y = "ER")

# Arrange the plots in a 2x2 grid
library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)# Using ggplot2 for a 2x2 grid of plots



data <- data[-c(1:4), ]

# Define the variables to test
variables <- c("epi_growth", "gdp_growth", "tax_growth", "er_growth")

# Loop over the variables and remove the first 4 values
for (var in variables) {
  data[[var]] <- na.omit(data[[var]])
}


# Initialize a list to store results
adf_results_list <- list()

# Loop over variables and perform ADF tests
for (variable in variables) {
  series <- data[[variable]]
  
  # Perform ADF test
  test_result <- adf.test(series, alternative = "stationary")
  
  # Store results
  adf_results_list[[variable]] <- data.frame(
    Variable = variable,
    Test_Statistic = test_result$statistic,
    P_Value = test_result$p.value
  )
}

# Combine results from all variables into a single data frame
adf_results <- do.call(rbind, adf_results_list)
adf_results



# ===============================================
# --------------- OLS regression ----------------
# ===============================================

install.packages("broom")
library(broom)
library(dplyr)
library(kableExtra)

data$tax_growth_lag <- c(NA, data$tax_growth[-length(data$tax_growth)])

models <- list(
  model1 = lm(tax_growth ~ tax_growth_lag, data = data),
  model2 = lm(tax_growth ~ tax_growth_lag + gdp_growth, data = data),
  model3 = lm(tax_growth ~ tax_growth_lag + gdp_growth + epi_growth, data = data),
  model4 = lm(tax_growth ~ tax_growth_lag + gdp_growth + epi_growth+ er_growth, data = data)
)

results <- lapply(models, tidy)
names(results) <- paste("Model", 1:length(results), sep = "_")

# Calculate R-squared values
r_squared <- sapply(models, function(model) summary(model)$r.squared)

# Append R-squared to each model's results
for (i in 1:length(models)) {
  model_name <- names(results)[i]
  n_coeff <- nrow(results[[model_name]])
  results[[model_name]]$r_squared <- rep(r_squared[i], n_coeff)
}

# Combine the results into a single data frame
results_df <- do.call("rbind", results)

# Format the results table as needed
results_df <- transform(results_df, term = as.character(term), p.value = format.pval(p.value, digits = 3))

# View the results
print(results_df)


# ===============================================
# ------------- ARIMA Forecasting ---------------
# ===============================================

fit_arma_model <- function(time_series, history_dates) {
  model <- auto.arima(time_series)
  forecast_data <- forecast(model, h = 6)
  
  # Historical data from 2010 onwards
  historical_df <- data.frame(
    Date = history_dates[history_dates >= as.Date("2010-01-01")],
    Point.Forecast = time_series[history_dates >= as.Date("2010-01-01")],
    Lo.95 = NA,  # Add NA columns for consistency
    Hi.95 = NA
  )
  
  # Forecast data
  forecast_df <- data.frame(
    Date = seq(tail(history_dates, 1) + 1, by = "1 year", length.out = 6),
    Point.Forecast = forecast_data$mean,
    Lo.95 = forecast_data$lower[, "95%"],
    Hi.95 = forecast_data$upper[, "95%"]
  )
  
  # Combine historical and forecast data
  combined_df <- rbind(historical_df, forecast_df)
  
  # Create residuals dataframe with correct dates
  residuals_df <- data.frame(
    Date = history_dates[1:length(residuals(model))],
    Residuals = residuals(model)
  )
  
  return(list(model = model, residuals = residuals_df, forecast = combined_df))
}

# Apply the function to each series
results <- list(
  epi = fit_arma_model(data$epi_growth, data$dates),
  gdp = fit_arma_model(data$gdp_growth, data$dates),
  tax = fit_arma_model(data$tax_growth, data$dates),
  er = fit_arma_model(data$er_growth, data$dates)
)

# Variable names for y labels
var_names <- c("EPI", "GDP", "TAX", "ER")

# Plotting
plot_list <- list()
for (i in 1:length(results)) {
  var <- names(results)[i]
  
  # Residuals plot with corrected column name
  p1 <- ggplot(results[[var]]$residuals, aes(x = Date, y = Residuals)) +
    geom_line() +
    theme_minimal() +
    theme(axis.title.x = element_blank()) +  # Remove x-axis label
    ylab(var_names[i]) +  # Y-axis label for residuals
    scale_x_date(date_breaks = "5 years", date_labels = "%Y")
  
  # Add title only to the first residuals plot
  if (i == 1) {
    p1 <- p1 + ggtitle("Residuals")
  }
  
  # Forecast plot
  p2 <- ggplot(results[[var]]$forecast, aes(x = Date, y = Point.Forecast)) +
    geom_line() +
    geom_ribbon(aes(ymin = Lo.95, ymax = Hi.95), alpha = 0.2) +
    theme_minimal() +
    theme(axis.title.x = element_blank()) +  # Remove x-axis label
    ylab("") +  # No Y-axis label for forecast
    scale_x_date(date_breaks = "5 years", date_labels = "%Y")
  
  # Add title only to the first forecast plot
  if (i == 1) {
    p2 <- p2 + ggtitle("Out-Of-Sample Forecast")
  }
  
  # Adding plots to the list
  plot_list[[i * 2 - 1]] <- p1
  plot_list[[i * 2]] <- p2
}

do.call(grid.arrange, c(plot_list, ncol = 2))


# Loop through each series in the results and print the model
for (var in names(results)) {
  cat("\nModel for", var, ":\n")
  print(results[[var]]$model)
}


# ===============================================
# --------------- VAR Modeling -- ---------------
# ===============================================

# select variables used in the VAR
myvar<- subset(data, select = c(epi_growth, gdp_growth, tax_growth, er_growth))

# estimate a reduced form var (assume 2 lag)
var.est1 <- VAR(myvar, p = 2, type = "const", season = NULL)
summary(var.est1)

# Assuming bvaroutput is your estimated BVAR model
bvaroutput <- bvar(myvar, lags = 2, n_draw = 30000L, n_burn = 5000L, verbose = TRUE, priors = bv_priors(hyper = "auto", mn = bv_mn(b = c(0.75,0.75,0.75,0.75))))
bvarirf    <- irf(bvaroutput, bv_irf(horizon = 20L, identification = TRUE), n_thin = 5L)

par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(irf(bvaroutput), vars_response = "pib_ae", vars_impulse = "epu_ae_new")

variables <- c("epi_growth", "gdp_growth", "tax_growth", "er_growth")

for (var in variables) {
  # Plot IRF for each variable in response to a shock in epi_growth
  plot(irf(bvaroutput), vars_response = var, vars_impulse = "epi_growth")
}







