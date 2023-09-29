library(tidyverse)
library(score4cast)
library(lubridate)
source('R/score_weather.R')


# want to score all this years forecasts
# 1. Check the date
dates <- seq.Date(as_date('2023-01-01'), Sys.Date(), 'day')

# 2. Check if the forecast has already been scored
scored_forecasts <-
  arrow::open_dataset('scores') |>
  filter(model_id == 'NOAA') |>
  distinct(reference_datetime, site_id) |>
  collect() |>
  mutate(reference_datetime = as_date(reference_datetime))

# 3. Generate list of forecasts to be scored
aquatic_sites <- arrow::open_dataset('scores') |>
  distinct(site_id) |>
  pull()

site_dates <- expand.grid(site_id = aquatic_sites, 
                          reference_datetime = dates)

to_score <- site_dates |> 
  anti_join(scored_forecasts)

# Loop through all site-date combos to score
for (i in 1:nrow(to_score)) {
  # get the forecast
  forecast_date <- to_score$reference_datetime[i]
  site <- to_score$site_id[i]

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
    summarise(prediction = mean(prediction), .groups = 'drop') |>
    rename(observation = prediction)


  scores <- score_weather_forecast(forecast = stage_2, target = stage_3) |>
    mutate(model_id = 'NOAA',
           date = as_date(datetime))

  arrow::write_dataset(scores, path = 'scores', format = 'parquet',
                       partitioning=c("model_id", "site_id", "reference_datetime"))
  message(i, ' ---- scored ', site, ' ', forecast_date, ' ----')
}
