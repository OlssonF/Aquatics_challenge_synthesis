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
forecast_dates <- as.character(seq.Date(as_date('2023-01-01'), Sys.Date(), 'day'))
variables <- c('temperature', 'oxygen', 'chla')

# Get a list of model_id and reference_datetime combinations that have
  # been scored since the beginning of the year
this_year <- scores_s3 |>
  arrow::open_dataset() |>
  filter(reference_datetime %in% forecast_dates,
         model_id %in% model_meta$model_id) |>
  distinct(model_id, reference_datetime) |>
  collect() 

# calculate the total number of submissions and the most recent and filter to only ones with enough to compare
  # at least half of the forecast reference_datetimes specified

submissions <- this_year |>
  mutate(reference_datetime = as_date(reference_datetime)) |>
  group_by(model_id) |>
  summarise(most_recent = max(reference_datetime),
            n_submissions = n()) |>
  filter(n_submissions >= length(forecast_dates)/3)


# xgboost_ids <- unique(submissions$model_id)[str_detect(unique(submissions$model_id), 'xgboost')]
# omit <- c(xgboost_ids[2:length(xgboost_ids)],
#           'null')

submissions |>
  # filter(model_id %nin% omit) |>
  write_csv('recent_submissions.csv')
#######################################

# Build/update local database using the recent_submissions

# Grab the data from the score bucket
scores_s3 <- arrow::s3_bucket("neon4cast-scores/parquet/aquatics/",
                              endpoint_override= "data.ecoforecast.org", anonymous = T)


# build a local parquet database from which to query
scores_s3 |>
  arrow::open_dataset() |>
  dplyr::filter(reference_datetime %in% forecast_dates,
                variable %in% variables,
                model_id %in% model_meta$model_id) |>
  arrow::write_dataset("scores", partitioning=c("model_id", "site_id"))

#====================================================================#
