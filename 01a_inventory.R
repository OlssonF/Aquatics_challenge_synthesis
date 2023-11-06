library(tidyverse)
# inventory
googlesheets4::gs4_deauth()
model_meta <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1oC7_w63wSCXNiHs1IK8AFGr0MG-NdjDAjwkfjvRZW-I/edit?usp=sharing") |> 
  mutate(unc_driver = ifelse(uses_NOAA == 1, 1, 0))

# types of uncertainty represented
# Number of models that contain each type
model_meta |> 
  summarise(across(contains('unc'), ~ sum(.x, na.rm = T)))

# How many sources are represented in each model?
model_meta |> 
  select(model_id, contains('unc_')) |> 
  mutate(unc_n = rowSums(pick(where(is.numeric)))) |> 
  group_by(unc_n) |> 
  summarise(n = n())

# Which weather variables are used in each model?
NOAA_vars <- c('air_temperature',
               'air_pressure',
               'relative_humidity',
               'surface_downwelling_longwave_flux_in_air',
               'surface_downwelling_shortwave_flux_in_air', 
               'precipitation_flux',
               'eastward_wind',
               'northward_wind')

NOAA_summary <- model_meta |> 
  select(model_id) 


for (i in 1:length(NOAA_vars)) {
  detect_var <- NOAA_vars[i]
  
  NOAA_summary <- 
    NOAA_summary |> 
    mutate(detect_var = str_detect(model_meta$NOAA_var, detect_var)) 
  
  colnames(NOAA_summary)[1 + i] <- detect_var
}

# Number of models that contain each variable
NOAA_summary |> 
  select(-model_id) |> 
  summarise_all(~sum(.x, na.rm = T))

NOAA_summary |> 
  mutate(vars_n = rowSums(pick(where(is.logical)))) |> 
  group_by(vars_n) |> 
  summarise(n = n())

# Aspects of DA
# do the forecasts contain initial conditions?
model_meta |> 
  summarise(initial_cond = sum(initial_cond, na.rm = T))

# do the forecast models update parameters?
model_meta |> 
  summarise(updates_parameters = sum(updates_parameters, na.rm = T))

# is the forecast 'dynamic'?
model_meta |> 
  summarise(dynamic = sum(dynamic, na.rm = T))

# summary of submissions --------------------
scores <- arrow::open_dataset('scores')

distinct_submissions <- scores |> 
  distinct(model_id, site_id, variable) |> 
  collect()

distinct_submissions |> 
  group_by(model_id) |> 
  distinct(site_id) |> 
  summarise(site_n = n()) |> 
  group_by(site_n) |> 
  summarise(n())

distinct_submissions |> 
  group_by(model_id) |> 
  distinct(variable) |> 
  summarise(variable_n = n()) |> 
  group_by(variable_n) |> 
  summarise(n())

# models for temperature only
distinct_submissions |> 
  group_by(model_id) |> 
  distinct(variable) |> 
  mutate(n = 1) |> 
  pivot_wider(names_from = variable, values_from = n, values_fill = list(n=0)) |> 
  filter(temperature == 0 & chla == 1 & oxygen == 0) |> 
  ungroup() |> 
  summarise(n())