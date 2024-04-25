
# Open dataset-------------
# Dissolved oxygen
oxygen_scores <- arrow::open_dataset("./scores_v2") |>
  filter(variable == 'oxygen') |>
  collect()

# temperature
temperature_scores <- arrow::open_dataset("./scores_v2") |>
  filter(variable == 'temperature') |>
  collect()


# read in DOY scores ------------
# temp
temperature_climatology <- arrow::open_dataset("./scores_v2") |>
  filter(model_id == 'climatology', 
         variable == 'temperature') |>
  collect() |> 
  select(reference_datetime, datetime, crps, logs, site_id, model_id) |> 
  arrange(reference_datetime, datetime, site_id) |> 
  pivot_wider(values_from = c(crps, logs),
              names_from = model_id)

# DO
oxygen_climatology <- arrow::open_dataset("./scores_v2") |>
  filter(model_id == 'climatology', 
         variable == 'oxygen') |>
  collect() |> 
  select(reference_datetime, datetime, crps, logs, site_id, model_id) |> 
  arrange(reference_datetime, datetime, site_id) |> 
  pivot_wider(values_from = c(crps, logs),
              names_from = model_id)

#-------------------------#

# Calculate relative skill ------
temperature_skill <- temperature_scores |>
  filter(model_id != 'climatology') |> 
  select(reference_datetime, datetime, crps, logs, site_id, model_id) |> 
  full_join(temperature_climatology) |> 
  
  # calcaulte the skill relative to climatology (- is bad, + is good)
  mutate(skill_crps = crps_climatology - crps,
         skill_logs = logs_climatology - logs,
         horizon = as.numeric(as_date(datetime) - as_date(reference_datetime))) |> 
  # consistent forecast period
  filter(horizon <= 30,
         horizon >= 1, 
         horizon != 15) |> 
  arrange(reference_datetime, site_id, datetime, model_id) 


oxygen_skill <- oxygen_scores |>
  filter(model_id != 'climatology') |> 
  select(reference_datetime, datetime, crps, logs, site_id, model_id) |> 
  full_join(oxygen_climatology) |> 
  
  # calcaulte the skill relative to climatology (- is bad, + is good)
  mutate(skill_crps = crps_climatology - crps,
         skill_logs = logs_climatology - logs,
         horizon = as.numeric(as_date(datetime) - as_date(reference_datetime))) |> 
  # consistent forecast period
  filter(horizon <= 30,
         horizon >= 1, 
         horizon != 15) |> 
  arrange(reference_datetime, site_id, datetime, model_id) 
