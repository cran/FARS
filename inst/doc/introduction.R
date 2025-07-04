## -----------------------------------------------------------------------------

# Install FARS from CRAN
#install.packages("FARS")

# Install FARS from GitHub
#install.packages("devtools")
#devtools::install_github("GPEBellocca/FARS")

library(FARS)

## -----------------------------------------------------------------------------
library(zoo)
library(readxl)
# Input dep variable
data_input_path <- system.file("extdata", "Data_IMF.xlsx", package = "FARS")
data_input <- read_excel(data_input_path, sheet = "data")
data_ts <- ts(data_input[, -1], frequency = 4)
data_diff <- diff(log(data_ts)) * 400
dep_variable <- data_diff[, 'United States', drop = FALSE] 
dep_variable <- dep_variable[2:60]

# Input data
data_df_path <- system.file("extdata", "DataBase.xlsx", package = "FARS")
data_df <- openxlsx::read.xlsx(data_df_path, sheet = "fulldata", cols = 2:625)
data_df <- data_df[,1:519]
data <- as.matrix(data_df)
dimnames(data) <- NULL

# Generate dates
quarters <- as.yearqtr(seq(from = as.yearqtr("2005 Q2"), by = 0.25, length.out = 59))
dates <- as.Date(quarters)

# Correct outliers
outliers_result <- correct_outliers(data, 5 )
data <- outliers_result$data


## -----------------------------------------------------------------------------
# MULTI-LEVEL DYNAMIC FACTOR MODEL
# 3 blocks
n_blocks <- 3 # 63 248 208
block_ind <- c(63,311,519)
global = 1
local = c(1,1,1)
middle_layer <- list("1-3" = 1)


mldfm_result <- mldfm(data, 
                      globa = global,
                      local = local,
                      middle_layer = middle_layer,
                      blocks = n_blocks, 
                      block_ind = block_ind , 
                      tol = 1e-6, 
                      max_iter = 1000,
                      method = 0) 

# Plot factors
# plot(mldfm_result, dates = dates) 


## -----------------------------------------------------------------------------
# Subsampling
n_samples <- 100
sample_size <- 0.94
mldfm_subsampling_result <- mldfm_subsampling(data, 
                                         global = global, 
                                         local = local,
                                         middle_layer = middle_layer,
                                         blocks = n_blocks, 
                                         block_ind = block_ind , 
                                         tol = 1e-6, 
                                         max_iter = 1000, 
                                         method = 0,
                                         n_samples = n_samples,
                                         sample_size = sample_size,
                                         seed = 42) 

## -----------------------------------------------------------------------------
# Create stressed scenario
scenario <- create_scenario(model = mldfm_result,
                                  subsample = mldfm_subsampling_result,
                                  alpha=0.95,
                                  atcsr=FALSE)
                                      

## -----------------------------------------------------------------------------
# Compute Quantiles
fars_result <- compute_fars(dep_variable, 
                              mldfm_result$Factors, 
                              scenario = scenario, 
                              h = 1,   
                              edge = 0.05, 
                              min = TRUE,
                              QTAU = 0.05) 
# Plot quantiles
#plot(fars_result,dates=dates)

## -----------------------------------------------------------------------------
# Density 
density <- density(fars_result$Quantiles,  
                             levels = fars_result$Levels,  
                             est_points = 512, 
                             random_samples = 100000,
                             seed = 42)
# Plot density
#plot(density, time_index = dates)


## -----------------------------------------------------------------------------
#GaR
GaR <- quantile_risk(density, QTAU = 0.05)

## -----------------------------------------------------------------------------
# Scenario Density 
scenario_density <- density(fars_result$Stressed_Quantiles,  
                             levels = fars_result$Levels,  
                             est_points = 512, 
                             random_samples = 100000,
                             seed = 42)


## -----------------------------------------------------------------------------
#GiS
GiS0 <- quantile_risk(scenario_density, QTAU = 0.05)

