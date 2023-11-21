library(tidyverse)
library(score4cast)
library(lubridate)
library(neonUtilities)
source('R/score_weather.R')
source('R/ignore_sigpipe.R')

# --- stage2 vs stage3 ----
# want to score all this years forecasts
# 1. Check the date
dates <- seq.Date(as_date('2023-01-01'), (Sys.Date() - days(1)), 'day')

# 2. Check if the forecast has already been scored
scored_forecasts <-
  arrow::open_dataset('scores') |>
  filter(model_id == 'NOAA_daily') |>
  distinct(reference_datetime, site_id) |>
  collect() |>
  mutate(reference_datetime = as_date(reference_datetime))

# 3. Generate list of forecasts to be scored
lake_sites <- c('BARC', 'CRAM', 'LIRO', 'PRLA', 'PRPO', 'SUGG', 'TOOK')

site_dates <- expand.grid(reference_datetime = dates,
                          site_id = lake_sites)

to_score <- site_dates |> 
  anti_join(scored_forecasts) 

# forecast from a couple of dates are missing

to_score <- to_score |> 
  filter(!reference_datetime %in% as_date(c('2023-01-06', '2023-01-19', '2023-05-22')))

# Loop through all site-date combos to score
for (i in 1:nrow(to_score)) {
  # get the forecast
  forecast_date <- to_score$reference_datetime[i]
  site <- to_score$site_id[i]

  stage_2 <- neon4cast::noaa_stage2(start_date = forecast_date) |>
    filter(site_id == site,
           variable == 'air_temperature') |>
    collect()|> 
    mutate(datetime = as_date(datetime)) |> 
    group_by(parameter, reference_datetime, datetime, site_id, variable, family) |> 
    summarise(prediction = mean(prediction))

  # get the observations
  stage_3 <- neon4cast::noaa_stage3() |>
    filter(site_id  == site,
           datetime %in% unique(stage_2$datetime),
           variable == 'air_temperature') |>
    collect() |>

    # compare with the mean prediction (as if it were the observation)
    mutate(datetime = as_date(datetime)) |> 
    group_by(datetime, site_id, variable) |>
    summarise(prediction = mean(prediction), .groups = 'drop') |>
    rename(observation = prediction)


  scores <- score_weather_forecast(forecast = stage_2, target = stage_3) |>
    mutate(model_id = 'NOAA_daily',
           date = as_date(datetime))

  arrow::write_dataset(scores, path = 'scores', format = 'parquet',
                       partitioning=c("model_id", "site_id", "reference_datetime"))
  message(i, '/', nrow(to_score), ' ---- scored ', site, ' ', forecast_date, ' ----')
}

# --- stage 2 vs observations ----
# want to score all this years forecasts
# 1. Check the date
dates <- seq.Date(as_date('2023-01-01'), (Sys.Date() - days(1)), 'day')
lake_sites <- c('BARC', 'CRAM', 'LIRO', 'PRLA', 'PRPO', 'SUGG', 'TOOK')

# 2. Check if the forecast has already been scored
if (!dir.exists('scores/model_id=NOAA_daily_obs')) {
  dir.create(file.path('scores', 'model_id=NOAA_daily_obs'))
}

scored_forecasts <-
  arrow::open_dataset('scores') |>
  filter(model_id == 'NOAA_daily_obs', 
         site_id %in% lake_sites) |>
  distinct(reference_datetime, site_id) |>
  collect() |>
  mutate(reference_datetime = as_date(reference_datetime))

# 3. Generate list of forecasts to be scored
# aquatic_sites <- arrow::open_dataset('scores') |>
#   distinct(site_id) |>
#   pull()

site_dates <- expand.grid(site_id = lake_sites,
                          reference_datetime = dates)

to_score <- site_dates |> 
  anti_join(scored_forecasts) 

# forecast from a couple of dates are missing

to_score <- to_score |> 
  filter(!reference_datetime %in% as_date(c('2023-01-06', '2023-01-19', '2023-05-22'))) 

# Loop through all site-date combos to score
for (i in 1:nrow(to_score)) {
  # get the forecast
  forecast_date <- to_score$reference_datetime[i]
  site <- to_score$site_id[i]
  
  stage_2 <- neon4cast::noaa_stage2(start_date = forecast_date) |>
    filter(site_id == site, 
           variable == 'air_temperature') |> # scoring AT only
    collect()|> 
    mutate(datetime = as_date(datetime)) |> 
    group_by(parameter, reference_datetime, datetime, site_id, variable, family) |> 
    summarise(prediction = mean(prediction))
  
  # get the observations
  obs_start <- format(forecast_date, "%Y-%m")
  obs_end <- format(forecast_date + days(35), "%Y-%m")
  
  neon_obs <- try(loadByProduct(dpID="DP1.20046.001", site = site,
                                startdate=obs_start, enddate=obs_end, 
                                package="expanded", 
                                token = Sys.getenv("NEON_TOKEN"),
                                check.size = F,
                                release = "current"), silent = T)
  
  if (class(neon_obs) != 'try-error') {
    
    # unlist the variables and add to the global environment
    list2env(neon_obs, .GlobalEnv)
    
    #format data
    neon_obs_dat <- RHbuoy_30min %>%
      select(siteID, startDateTime, tempRHMean) %>%
      mutate(datetime = as_date(startDateTime)) %>%
      group_by(siteID, datetime) |> 
      summarise(observation = mean(tempRHMean), .groups = 'drop') |> 
      rename(site_id = siteID) %>%
      select(site_id, datetime, observation) |> 
      mutate(variable = 'air_temperature',
             observation = observation + 273.15)
    
    
    scores <- score_weather_forecast(forecast = stage_2, target = neon_obs_dat) |>
      mutate(reference_datetime = as_date(reference_datetime),
             model_id = 'NOAA_daily_obs',
             date = as_date(datetime))
    
    arrow::write_dataset(scores, path = 'scores', format = 'parquet',
                         partitioning=c("model_id", "site_id", "reference_datetime"))
    message(i, '/', nrow(to_score), ' ---- scored ', site, ' ', forecast_date, ' ----')
  } else {
    scores <- data.frame(reference_datetime = forecast_date,
                         site_id = site,
                         datetime = seq.Date(forecast_date, forecast_date + days(35), 'day'),
                         model_id = 'NOAA_daily_obs')
    arrow::write_dataset(scores, path = 'scores', format = 'parquet',
                         partitioning=c("model_id", "site_id", "reference_datetime"))
    
    message(i, '/', nrow(to_score), '---- ', site, ' ', forecast_date, ' not scored ----')
    
  }
  
}
