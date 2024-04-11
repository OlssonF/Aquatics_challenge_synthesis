library(tidyverse)
# inventory
googlesheets4::gs4_deauth()
model_meta <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1oC7_w63wSCXNiHs1IK8AFGr0MG-NdjDAjwkfjvRZW-I/edit?usp=sharing") |> 
  mutate(unc_driver = ifelse(uses_NOAA == 1, 1, 0)) 

lake_sites <- c('BARC', 'CRAM', 'LIRO', 'PRLA', 'PRPO', 'SUGG', 'TOOK')
# How many models?
model_meta |>
  mutate(tw = str_detect(variable, 'temperature'),
         do = str_detect(variable, 'oxygen'),
         chla = str_detect(variable, 'chla')) |> 
  summarise(sum(tw), sum(do), sum(chla))

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

model_meta |> 
  filter(str_detect(variable, 'chla')) |> 
  group_by(model_type) |> 
  summarise(n())


# How many forecasts?
all_lake_forecasts <- arrow::open_dataset('scores_2023') |> 
  filter(model_id %in% model_meta$model_id,
         site_id %in% lake_sites) |> 
  distinct(reference_datetime, variable, 
           site_id, model_id) |> 
  collect()

all_lake_forecasts |> 
  filter(variable == 'temperature') |> 
  summarise(n())

# use AT as a covariate?
model_meta |> 
  select(model_id, NOAA_var, variable) |>
  mutate(tw = str_detect(variable, 'temperature'),
         do = str_detect(variable, 'oxygen'),
         chla = str_detect(variable, 'chla')) |> 
  filter(str_detect(NOAA_var, 'air_temp')) |>
  summarise(sum(tw), sum(do), sum(chla))




# Which weather variables are used in each model?
NOAA_vars <- c('air_temperature',
               'air_pressure',
               'relative_humidity',
               'surface_downwelling_longwave_flux_in_air',
               'surface_downwelling_shortwave_flux_in_air', 
               'precipitation_flux',
               'eastward_wind',
               'northward_wind')

NOAA_summary <- wt_meta |> 
  select(model_id, variable) 


for (i in 1:length(NOAA_vars)) {
  detect_var <- NOAA_vars[i]
  
  NOAA_summary <- 
    NOAA_summary |> 
    mutate(detect_var = str_detect(wt_meta$NOAA_var, detect_var)) 
  
  colnames(NOAA_summary)[2 + i] <- detect_var
}

# Number of models that contain each variable
NOAA_summary |> 
  select(-model_id) |>
  group_by(variable) |> 
  summarise_all(~sum(.x, na.rm = T))

NOAA_summary |> 
  mutate(vars_n = rowSums(pick(where(is.logical)))) |> 
  group_by(variable, vars_n) |> 
  summarise(n = n())


# looking at drivers by variable:
wt_meta |> 
  mutate(n_noaa = length(str_split_1(string = NOAA_var, pattern = ',')))

wt_meta |> group_by(model_type) |> 
  summarise(n())




# types of uncertainty represented
# Number of models that contain each type
wt_meta <- model_meta |> 
  filter(str_detect(variable, 'temperature'))
wt_meta |> 
  summarise(across(contains('unc'), ~ sum(.x, na.rm = T)))

# How many sources are represented in each model?
wt_meta |> 
  select(model_id, contains('unc_')) |> 
  mutate(unc_n = rowSums(pick(where(is.numeric)))) |> 
  group_by(unc_n) |> 
  summarise(n = n())

