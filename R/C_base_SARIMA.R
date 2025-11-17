library(forecast)

# build monthly time series
ts_cases <- ts(base_df$cases, start = c(2012, 1), frequency = 12)

# train data on whole dataset
fit_sarima <- auto.arima(
  ts_cases,
  seasonal = TRUE,
  stepwise = FALSE,   # better search
  approximation = FALSE
)

summary(fit_sarima)
# Series: ts_cases 
# ARIMA(1,1,2) 
# 
# Coefficients:
#           ar1     ma1     ma2
#       -0.5985  0.8808  0.6814
# s.e.   0.0915  0.0724  0.0766
# 
# sigma^2 = 407.3:  log likelihood = -578.67
# AIC=1165.33   AICc=1165.65   BIC=1176.83
# 
# Training set error measures:
#                      ME    RMSE      MAE       MPE     MAPE      MASE        ACF1
# Training set 0.04292172 19.8741 11.75774 -1.057733 18.20849 0.2917553 -0.04024197

# Plot the residuals
checkresiduals(fit_sarima)

# a. top bit 
# The residuals fluctuate around 0, which is what we want — this means the model captures the general trend and seasonal patterns reasonably well.
# There are large spikes around 2021–2022, caused by the extreme outbreak in early 2021. These represent systematic underprediction: the model cannot anticipate the sudden outbreak peaks.
# There is some clustering of larger residuals around outbreak periods, suggesting mild heteroscedasticity (non-constant variance), especially in 2021 when variance increases.

# b. bottom left
# Most autocorrelation bars are within the blue confidence bands, which is what we want. This means the residuals behave roughly like white noise — SARIMA captures most of the serial correlation.
# There are mild spikes at lag 12 and 24, suggesting a small amount of remaining annual seasonality. This means the model may be slightly underfitting seasonal structure.

# c. bottom right
# The distribution is slightly right-skewed, indicating a deviation from the normality assumption (SARIMA assumes residuals are approximately normally distributed).
# Most residuals are close to 0, so overall the shape is not too bad.
# There are a few large outliers, caused by outbreak months. These contribute to the skewness and non-normality.


# Fit the next time period
fc <- forecast(fit_sarima, h = 36)
plot(fc, main = "SARIMA Forecast: Influenza Cases")

