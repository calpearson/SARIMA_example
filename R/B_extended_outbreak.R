
# Create additional years 2023-2025
new_years <- 2023:2025
new_months <- rep(1:12, length(new_years))

# Placeholder cases (set as NA or simulate)
new_cases <- rep(NA, length(new_months))

# No outbreaks assumed by default
new_outbreak <- rep(0, length(new_months))

# Create new data frame
df_new <- data.frame(
  year = rep(new_years, each = 12),
  month = new_months,
  cases = new_cases,
  outbreak = new_outbreak
)

# Add corresponding date column
df_new$date <- as.Date(paste(df_new$year, df_new$month, "01", sep = "-"))

# bind them together
df_extended <- rbind(df, df_new)


#...............................................................................
#...............................................................................
### MANUAL SECTION ----
#...............................................................................
#...............................................................................

# Explain: Right you will here have to calculate the average distance in months between end of outbreak 1 and start of outbreak 2 then end of outbreak 2 to start of outbreak 3.

# Outbreak diff time
# I found that the mean distance is 44 months after the end of the previous outbreak
# (43+44)/2 = 43.5 i.e. 44

# Outbreak duration
# I found out that the mean outbreak duration is
# (4+4+10)/3 = 6

df_extended <- df_extended %>% 
  mutate(date = as.Date(paste(year, month, "01", sep = "-")),
         # average time is 6 months
         outbreak = case_when(date == as.Date("2024-12-01") ~ 1,
                              date == as.Date("2025-01-01") ~ 1,
                              date == as.Date("2025-02-01") ~ 1,
                              date == as.Date("2025-03-01") ~ 1,
                              date == as.Date("2025-04-01") ~ 1,
                              date == as.Date("2025-05-01") ~ 1,
                              TRUE ~ outbreak),
         # all the others needs to be 0 not NA this stopps the other bits from being transformed.
         outbreak = ifelse(is.na(outbreak), 0, outbreak))

#...............................................................................


# rename
base_df <- df
full_df <- df_extended
chunk_df <- df_new

# clean house
rm(df,df_extended,df_new)

# plot base
plot(base_df$date, base_df$cases, type = "l",
     xlab = "Month",
     ylab = "Influenza Cases",
     main = "Influenza Cases Over Time")

# plot base
plot(full_df$date, full_df$cases, type = "l",
     xlab = "Month",
     ylab = "Influenza Cases",
     main = "Influenza Cases Over Time")