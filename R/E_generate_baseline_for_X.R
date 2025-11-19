# --- Load libraries ---
library(dplyr)
library(ggplot2)
library(forecast)

# --- 1. PREPARE DATA ------------------------------------------------------

full_df <- full_df %>% arrange(date)
full_df$month_index <- seq_len(nrow(full_df))

# --- 2. SELECT NON-OUTBREAK MONTHS ---------------------------------------

train_df <- subset(full_df, outbreak == 0)

# --- 3. FIT SARIMA BASELINE MODEL ON NON-OUTBREAK MONTHS ------------------

fit_baseline <- auto.arima(train_df$cases,
                           seasonal = TRUE,
                           stepwise = FALSE,
                           approximation = FALSE)

# Baseline forecast for all months
baseline_forecast <- forecast(fit_baseline, h = nrow(full_df))$mean
full_df$baseline <- as.numeric(baseline_forecast)

# --- 4. CALCULATE EXCESS CASES -------------------------------------------

full_df$excess <- full_df$cases - full_df$baseline

# --- 5. FIT LINEAR TREND (COEFFICIENT LINE) --------------------------------

# Linear trend using non-outbreak months only
lm_non_outbreak <- lm(cases ~ month_index, data = train_df)
full_df$trend_no_outbreak <- predict(lm_non_outbreak, newdata = full_df)

# --- 6. PLOT OBSERVED, BASELINE, TREND, AND EXCESS ------------------------

ggplot(full_df, aes(x = date)) +
  geom_line(aes(y = cases, color = "Observed cases"), size = 0.7) +
  geom_line(aes(y = baseline, color = "SARIMA baseline"), size = 1, linetype = "dashed") +
  geom_line(aes(y = trend_no_outbreak, color = "Linear trend"), size = 1, linetype = "dotdash") +
  geom_ribbon(aes(ymin = baseline, ymax = cases), fill = "purple", alpha = 0.3) +
  labs(
    title = "Observed Cases with SARIMA Baseline, Linear Trend, and Excess",
    subtitle = "Baseline = SARIMA (non-outbreak months), Trend = linear fit on non-outbreak months",
    y = "Cases",
    x = "Date",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_color_manual(values = c(
    "Observed cases" = "black",
    "SARIMA baseline" = "red",
    "Linear trend" = "blue"
  ))

# --- 7. DIAGNOSTICS --------------------------------------------------------

cat("SARIMA baseline model:\n")
print(fit_baseline)

cat("\nLinear trend coefficients (Intercept, Slope per month):\n")
print(coef(lm_non_outbreak))

cat("\nFirst 10 rows with baseline, trend, and excess:\n")
print(head(full_df, 10))



# --- Generate difference between linear trend without outbreak against observed ------

full_df <- full_df %>%
  mutate(exceedance = cases-trend_no_outbreak)

