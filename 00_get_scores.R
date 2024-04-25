
library(tidyverse)
library(lubridate)

options(timeout = 1000)

# Get from data publication on Zenodo
save_loc <- here::here()

download.file(url = "https://sandbox.zenodo.org/records/48909/files/scores_v2.zip?download=1",
              destfile = file.path(save_loc,"scores.zip"))
#If this doesn't work try adding the method = "curl" argument

unzip(file.path(save_loc,"scores.zip"))



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
# variables <- c('temperature', 'oxygen', 'chla')
# sites <- c('BARC', 'CRAM', 'LIRO', 'PRLA', 'PRPO', 'SUGG', 'TOOK')
# 
# 
# bad_forecasts <- expand.grid(model_id = c('flareGOTM','flareGOTM_noDA', 'flare_ler', 'flare_ler_baseline'),
#                              site_id = "TOOK")
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
