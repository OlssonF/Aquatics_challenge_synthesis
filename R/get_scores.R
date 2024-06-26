
library(tidyverse)
library(lubridate)

options(timeout = 1000)

# Get from data publication on Zenodo

download.file(url = "https://zenodo.org/records/11087208/files/scores.zip?download=1",
              destfile = "scores.zip")
download.file(url = "https://zenodo.org/records/11087208/files/targets.zip?download=1",
              destfile = "targets.zip")


#If this doesn't work try adding the method = "curl" argument
unzip("scores.zip")
unzip("targets.zip")


# if using S3...
# scores_s3 <- arrow::s3_bucket("neon4cast-scores/parquet/aquatics/",
#                               endpoint_override= "data.ecoforecast.org", anonymous = T)
# googlesheets4::gs4_deauth()
# model_meta <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1oC7_w63wSCXNiHs1IK8AFGr0MG-NdjDAjwkfjvRZW-I/edit?usp=sharing") |>
#   mutate(unc_driver = ifelse(uses_NOAA == 1, 1, 0))
# 
# 
# # What and when?
# forecast_dates <- as.character(seq.Date(as_date('2023-01-01'), as_date('2023-12-31'), 'day'))
# variables <- c('temperature', 'oxygen')
# sites <- c('BARC', 'CRAM', 'LIRO', 'PRLA', 'PRPO', 'SUGG', 'TOOK')
# 
# 
# bad_forecasts <- expand.grid(model_id = c('flareGOTM','flareGOTM_noDA', 'flare_ler', 'flare_ler_baseline'),
#                              site_id = "TOOK",
#                              variable = 'temperature')
# # build a local parquet database from which to query
# scores_s3 |>
#   arrow::open_dataset() |>
#   dplyr::filter(reference_datetime %in% forecast_dates,
#                 variable %in% variables,
#                 site_id %in% sites,
#                 model_id %in% model_meta$model_id) |>
#   anti_join(bad_forecasts) |>
#   arrow::write_dataset("scores", partitioning=c("model_id"))

#====================================================================#
