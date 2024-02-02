# Get model_id for this years submissions

library(tidyverse)
library(lubridate)
`%nin%` = Negate(`%in%`)

scores_s3 <- arrow::s3_bucket("neon4cast-scores/parquet/aquatics/",
                              endpoint_override= "data.ecoforecast.org", anonymous = T)

googlesheets4::gs4_deauth()
model_meta <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1oC7_w63wSCXNiHs1IK8AFGr0MG-NdjDAjwkfjvRZW-I/edit?usp=sharing") |> 
  mutate(unc_driver = ifelse(uses_NOAA == 1, 1, 0))


# What and when?
forecast_dates <- as.character(seq.Date(as_date('2023-01-01'), as_date('2023-12-31'), 'day'))
variables <- c('temperature', 'oxygen', 'chla')


# build a local parquet database from which to query
scores_s3 |>
  arrow::open_dataset() |>
  dplyr::filter(reference_datetime %in% forecast_dates,
                variable %in% variables,
                model_id %in% model_meta$model_id) |>
  arrow::write_dataset("scores_2023", partitioning=c("model_id", "site_id"))

#====================================================================#
