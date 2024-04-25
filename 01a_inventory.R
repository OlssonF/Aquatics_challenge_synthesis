library(tidyverse)


# Model submissions -------------------------------------------------------
# Read in the google sheet of model descriptions
googlesheets4::gs4_deauth()
model_meta <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/16J_jBbYyMoPbJujBHnKbANXKIMCcu2UtjDHwnL5mUA0/edit?usp=sharing") |> 
  mutate(unc_driver = ifelse(uses_NOAA == 1, 1, 0)) 

lake_sites <- c('BARC', 'CRAM', 'LIRO', 'PRLA', 'PRPO', 'SUGG', 'TOOK')

# How many models?
model_meta |>
  mutate(tw = str_detect(variable, 'temperature'),
         do = str_detect(variable, 'oxygen')) |> 
  summarise(sum(tw), sum(do))

model_meta |> 
  group_by(variable) |> 
  summarise(n())


# model classes by variable
model_meta |> 
  filter(str_detect(variable, 'temperature')) |> 
  group_by(model_type) |> 
  summarise(n())

model_meta |> 
  filter(str_detect(variable, 'oxygen')) |> 
  group_by(model_type) |> 
  summarise(n())


# use AT as a covariate?
model_meta |> 
  select(model_id, NOAA_var, variable) |>
  mutate(tw = str_detect(variable, 'temperature'),
         do = str_detect(variable, 'oxygen')) |> 
  filter(str_detect(NOAA_var, 'air_temp')) |>
  summarise(sum(tw), sum(do))


# types of uncertainty represented
# Number of models that contain each type
model_meta |>
  mutate(tw = str_detect(variable, 'temperature'))  |> 
  filter(tw == T) |> 
  summarise(across(contains('unc'), ~ sum(.x, na.rm = T)))



# How many sources are represented in each model?
model_meta |>
  mutate(tw = str_detect(variable, 'temperature'))  |> 
  filter(tw == T) |> 
  select(model_id, contains('unc_')) |> 
  mutate(unc_n = rowSums(pick(where(is.numeric)))) |> 
  group_by(unc_n) |> 
  summarise(n = n())


# Scores database summary -------------------------------------------------
# total forecasts - distinct model/site/var/refdate combos
distinct_forecasts <- arrow::open_dataset("./scores_v2") |> 
  distinct(model_id, site_id, variable, reference_datetime) |> 
  collect()

# variable forecasts
distinct_forecasts |> 
  group_by(variable ) |>
  summarise(n = n()) |> 
  mutate(percent = 100*(n/nrow(distinct_forecasts)))
