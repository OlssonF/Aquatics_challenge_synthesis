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

# look to see what forecasts have already been generated
out_format <- 'parquet'
out_dir = 'Forecasts/parquet'

if (out_format == 'parquet') {
  done <- open_dataset(out_dir) |> 
    distinct(reference_datetime) |> 
    collect()
} else {
  done_files <- list.files(out_dir, pattern = out_format)
  dates <- map_chr(.x = done_files, ~paste(str_split_1(string = .x, pattern = '-')[2],
                                           str_split_1(string = .x, pattern = '-')[3], 
                                           str_split_1(string = .x, pattern = '-')[4], sep = '-')
  )
  done <- data.frame(reference_datetime = dates)               
  
}

# skips ones already done
forecast_dates <- forecast_dates[-which(as.character(forecast_dates) %in% done$reference_datetime)]

for (i in 1:length(forecast_dates)) {
  date <- forecast_dates[i]
  
  # generate an MME using all 'available' forecasts
  mme_file <- create_all_mme(forecast_models = 'all',
                             ensemble_name = 'all_submissions',
                             forecast_date = date,
                             var = 'temperature', 
                             h = 30, 
                             theme = 'aquatics',
                             n = 200, 
                             omit = 'tg_auto_adam', 
                             out_dir = out_dir,
                             out_format = out_format)
  if (length(mme_file) == 0) {
    message(date, ' failed')
  }
}
#-------------------------------#

#--------- Scoring --------------
source('R/scoring_function_arrow.R')

score_path <- 'scores_rescored'

if (!dir.exists(score_path)) {
  dir.create(score_path, recursive = T)
}


#### Read in forecasts ####
# Open the dataset of forecast parquets to be scored
forecast_parquets <- './Forecasts/parquet'
open_parquets <- arrow::open_dataset(forecast_parquets)


# forecasts to scores
models <- 'all_submissions'

# vector of unique reference_datetimes
unique_reftime <- open_parquets |>
  distinct(reference_datetime) |>
  collect() |>
  arrange(reference_datetime) |>
  pull(reference_datetime)

# get all combinations of model and reference_datetime
to_score <- expand.grid(model_id = models, reference_datetime = unique_reftime)


#### Regular scoring function ####
for (i in 1:nrow(to_score)) {
  # subset the model and reference datetime
  forecast_df <- open_parquets|>
    dplyr::filter(model_id == to_score$model_id[i],
                  reference_datetime == to_score$reference_datetime[i]) |>
    collect()

  if (nrow(forecast_df) != 0) {
    # Score the forecast
    generate_forecast_score_arrow(targets_file = 'https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz',
                                  forecast_df = forecast_df,
                                  local_directory = score_path)
    message(i, "/", nrow(to_score), " forecasts scored")


  } else {
    message('no forecast for ', to_score$model_id[i], ' ', to_score$reference_datetime[i] )
  }

}
#=============================#


