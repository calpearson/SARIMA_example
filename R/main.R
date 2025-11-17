## 0) Get started ----

# Source Cal_Functions.R from GitHub
source("https://raw.githubusercontent.com/calpearson/Cal_Functions/main/Cal_Functions.R")

# Source Cal_Packages.R from GitHub
source("https://raw.githubusercontent.com/calpearson/Cal_Functions/main/Cal_packages.R")



# 1. Generate the base data
source("R/A_gen_data.R")

# 2. Extend the time fields
source("R/B_extended_outbreak.R")

# 3. Generate your base SARIMA model
source("R/C_base_SARIMA.R")

# 4. Generate your SARIMAX model (including the outbreak variable)
source("R/D_base_SARIMAX.R")

# 5. Generate SARIMAX using baseline trend (outbreak binary variable should be scaled to show by how much its above or below the baseline)
source("R/E_SARIMAX_using_baseline.R")