
library(arrow)
library(tidyverse)
library(lubridate)

lake_sites <- c('BARC', 'CRAM', 'LIRO', 'PRLA', 'PRPO', 'SUGG')

# read in the metadata from the google sheet
model_meta <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1oC7_w63wSCXNiHs1IK8AFGr0MG-NdjDAjwkfjvRZW-I/edit?usp=sharing")

# Open dataset-------------
# start with lake temperatures
lake_scores <- arrow::open_dataset("scores") |>
  filter(site_id %in% lake_sites,
         model_id != 'climatology',
         variable == 'temperature') |>
  collect()

# Calculate skill scores ------------
climatology_scores <- arrow::open_dataset("scores") |>
  filter(model_id == 'climatology', 
         site_id %in% lake_sites,
         variable == 'temperature') |>
  collect() |> 
  select(reference_datetime, datetime, crps, logs, site_id, model_id) |> 
  arrange(reference_datetime, datetime, site_id) |> 
  pivot_wider(values_from = c(crps, logs),
              names_from = model_id)

lake_skill_scores <- lake_scores |>
  select(reference_datetime, datetime, crps, logs, site_id, model_id) |> 
  full_join(climatology_scores) |> 
  
  # calcaulte the skill relative to climatology (- is bad, + is good)
  mutate(skill_crps = crps_climatology - crps,
         skill_logs = logs_climatology - logs,
         horizon = as.numeric(as_date(datetime) - as_date(reference_datetime))) |> 
  # consistent forecast period
  filter(horizon <= 30,
         horizon >= 1,
         model_id != 'fARIMA') |> 
  arrange(reference_datetime, site_id, datetime, model_id) 

#-------------------------------------------

# Question