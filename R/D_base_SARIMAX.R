library(forecast)

# Extract observed data only
train_df <- full_df[!is.na(full_df$cases), ]

ts_cases <- ts(train_df$cases, start = c(2012,1), frequency = 12)
ts_outbreak_train <- ts(train_df$outbreak, start = c(2012,1), frequency = 12)

fit_sarimax <- auto.arima(
  ts_cases,
  xreg = ts_outbreak_train,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)
summary(fit_sarimax)

# Future outbreak values (the xreg for forecasting)
future_xreg <- full_df$outbreak[is.na(full_df$cases)]
h <- length(future_xreg)  # number of months to forecast

fc <- forecast(
  fit_sarimax,
  xreg = future_xreg,
  h = h
)

plot(fc, main = "SARIMAX Forecast Using full_df Outbreak Window")

full_df$forecast <- c(
  fit_sarimax$fitted,   # fitted values for 2012–2022
  fc$mean               # forecasted values for 2023–2025
)


# Goal to detect next outbreak peak
future_part <- full_df[is.na(full_df$cases), ]

peak_idx <- which.max(future_part$forecast)
peak_date <- future_part$date[peak_idx]
peak_value <- future_part$forecast[peak_idx]

cat("Predicted outbreak peak:", format(peak_date, "%Y-%m"), "\n")
cat("Predicted peak cases:", round(peak_value), "\n")


# nice plot.

plot_df <- full_df %>%
  mutate(
    type = ifelse(is.na(cases), "Forecast", "Observed")
  )

plot_base_sarimax <- ggplot(plot_df, aes(x = date)) +
  
  # Observed cases
  geom_line(aes(y = cases), colour = "black", linewidth = 0.7) +
  
  # Forecast line
  geom_line(aes(y = forecast, colour = type), linewidth = 1) +
  
  # Outbreak shading
  geom_ribbon(
    data = plot_df[plot_df$outbreak == 1,],
    aes(ymin = -Inf, ymax = Inf),
    fill = "red", alpha = 0.08
  ) +
  
  labs(
    title = "SARIMAX Forecast of Influenza With Outbreak Indicator",
    y = "Cases",
    x = "Year"
  ) +
  
  scale_colour_manual(values = c("Observed" = "black", "Forecast" = "blue")) +
  
  theme_minimal(base_size = 13)

