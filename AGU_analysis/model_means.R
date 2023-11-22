# AGU analysis - what does the mean look like?
# take all submitted models (the spaghetti) and return the ensemble

# Submit all the ensemble forecast to challenge!
setwd(here::here())
library(tidyverse)
library(arrow)
library(lubridate)
source('./R/create_all_mme.R')
source('./R/get_forecast.R')

Sys.unsetenv("AWS_ACCESS_KEY_ID")
Sys.unsetenv("AWS_SECRET_ACCESS_KEY")
Sys.unsetenv("AWS_DEFAULT_REGION")
Sys.unsetenv("AWS_S3_ENDPOINT")
Sys.setenv(AWS_EC2_METADATA_DISABLED="TRUE")

# where are the raw forecasts
s3 <- "neon4cast-forecasts/parquet/aquatics/"

forecast_dates <- seq(as_date('2023-01-01'), Sys.Date(), 1)


for (i in 1:length(forecast_dates)) {
  date <- forecast_dates[1]
  
  mme_file <- create_all_mme(forecast_models = 'all',
                             ensemble_name = 'all_submissions',
                             forecast_date = date,
                             var = 'temperature', 
                             h = 30, 
                             theme = 'aquatics',
                             n = 200, 
                             omit = 'tg_auto_adam', 
                             out_dir = 'Forecasts/parquet',
                             output = 'parquet')
  
}
