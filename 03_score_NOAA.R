library(tidyverse)
library(score4cast)
source('R/score_weather.R')


# get the forecast
forecast_date <- '2023-01-01'
site <- 'BARC'

stage_2 <- neon4cast::noaa_stage2(start_date = forecast_date) |>
  filter(site_id == site) |>
  collect()

# get the observations
stage_3 <- neon4cast::noaa_stage3() |>
  filter(site_id  == site,
         datetime %in% unique(stage_2$datetime)) |>
  collect() |>

  # compare with the mean prediction (as if it were the observation)
  group_by(datetime, site_id, variable) |>
  summarise(prediction = mean(prediction)) |>
  rename(observation = prediction)


scores <- score_weather_forecast(forecast = stage_2, target = stage_3) |>
  mutate(model_id = 'NOAA',
         date = as_date(datetime))

arrow::write_dataset(scores, path = 'scores', format = 'parquet',
                     partitioning=c("model_id", "site_id"))
