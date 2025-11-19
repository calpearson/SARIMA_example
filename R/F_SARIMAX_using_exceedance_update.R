library(forecast)
library(dplyr)
library(ggplot2)

# -------------------------------
# Prepare full_df (your data)
# -------------------------------
# Assuming full_df already exists with columns:
#   date (Date), cases (numeric, NA for future), outbreak (0/1), exceedance (cases - trend_no_outbreak)
# For demonstration, here we assume full_df and train_df are ready
# full_df <- ... 
train_df <- subset(full_df, !is.na(cases))

# -------------------------------
# 1. Fit SARIMAX model
# -------------------------------
ts_cases <- ts(train_df$cases, start = c(2012,1), frequency = 12)
ts_exceedance <- ts(train_df$exceedance, start = c(2012,1), frequency = 12)

fit_sarimax <- auto.arima(
  ts_cases,
  xreg = ts_exceedance,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)

summary(fit_sarimax)

# -------------------------------
# 2. Forecast horizon
# -------------------------------
h <- sum(is.na(full_df$cases))
future_xreg <- rep(0, h)  # initialize

# Define future outbreak months
outbreak_dates <- as.Date(c("2024-12-01","2025-01-01","2025-02-01",
                            "2025-03-01","2025-04-01","2025-05-01"))
future_indices <- which(full_df$date[is.na(full_df$cases)] %in% outbreak_dates)

# -------------------------------
# 3. Option A: Mean historical exceedance
# -------------------------------
mean_exceedance <- mean(train_df$exceedance[train_df$outbreak == 1], na.rm = TRUE)
future_xreg_A <- future_xreg
future_xreg_A[future_indices] <- mean_exceedance

fc_A <- forecast(fit_sarimax, xreg = future_xreg_A, h = h)

# -------------------------------
# 4. Option B: Sample historical exceedances
# -------------------------------
set.seed(123)  # reproducibility
historical_excess <- train_df$exceedance[train_df$outbreak == 1]
future_xreg_B <- future_xreg
future_xreg_B[future_indices] <- sample(historical_excess,
                                        length(future_indices),
                                        replace = TRUE)

fc_B <- forecast(fit_sarimax, xreg = future_xreg_B, h = h)

# -------------------------------
# 5. Create plotting data frames
# -------------------------------
create_plot_df <- function(fit_sarimax, fc, full_df) {
  data.frame(
    date = full_df$date,
    observed = full_df$cases,
    forecast = c(fit_sarimax$fitted, fc$mean),
    lower95 = c(rep(NA, length(fit_sarimax$fitted)), fc$lower[,2]),
    upper95 = c(rep(NA, length(fit_sarimax$fitted)), fc$upper[,2]),
    type = ifelse(is.na(full_df$cases), "Forecast", "Observed")
  )
}

plot_df_A <- create_plot_df(fit_sarimax, fc_A, full_df)
plot_df_B <- create_plot_df(fit_sarimax, fc_B, full_df)

# -------------------------------
# 6. Plot Option A
# -------------------------------
plot_mean_historical_exceedance <- ggplot(plot_df_A, aes(x = date)) +
  geom_line(aes(y = observed), color = "black", size = 0.7) +
  geom_line(aes(y = forecast, color = "Forecast"), size = 1) +
  geom_ribbon(aes(ymin = lower95, ymax = upper95), fill = "blue", alpha = 0.2) +
  geom_ribbon(
    data = plot_df_A[full_df$outbreak == 1, ],
    aes(ymin = -Inf, ymax = Inf),
    fill = "red", alpha = 0.08
  ) +
  labs(title = "SARIMAX Forecast (Option A: Mean Historical Exceedance)",
       y = "Cases", x = "Year") +
  scale_color_manual(values = c("Forecast" = "blue")) +
  theme_minimal(base_size = 13)

# -------------------------------
# 7. Plot Option B
# -------------------------------
plot_sampled_historical_exceedance <- ggplot(plot_df_B, aes(x = date)) +
  geom_line(aes(y = observed), color = "black", size = 0.7) +
  geom_line(aes(y = forecast, color = "Forecast"), size = 1) +
  geom_ribbon(aes(ymin = lower95, ymax = upper95), fill = "green", alpha = 0.2) +
  geom_ribbon(
    data = plot_df_B[full_df$outbreak == 1, ],
    aes(ymin = -Inf, ymax = Inf),
    fill = "red", alpha = 0.08
  ) +
  labs(title = "SARIMAX Forecast (Option B: Sampled Historical Exceedances)",
       y = "Cases", x = "Year") +
  scale_color_manual(values = c("Forecast" = "green")) +
  theme_minimal(base_size = 13)



# -------------------------------
# 8. Plot Basic
# -------------------------------

plot_basic <- ggplot(plot_df, aes(x = date)) +
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





# -------------------------------
# 9. Make a function for the peaks
# -------------------------------
get_peak <- function(plot_df, outbreak_dates) {
  future_part <- plot_df[plot_df$type == "Forecast" & plot_df$date %in% outbreak_dates, ]
  peak_idx <- which.max(future_part$forecast)
  peak_date <- future_part$date[peak_idx]
  peak_value <- future_part$forecast[peak_idx]
  cat("Predicted outbreak peak:", format(peak_date, "%Y-%m"), "\n")
  cat("Predicted peak cases:", round(peak_value), "\n")
}

cat("Option A peak:\n")
plot_mean_historical_exceedance <- get_peak(plot_df_A, outbreak_dates)

cat("\nOption B peak:\n")
plot_sampled_historical_exceedance <- get_peak(plot_df_B, outbreak_dates)




