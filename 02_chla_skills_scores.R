
library(tidyverse)
library(arrow)
library(lubridate)
library(tidytext)

lake_sites <- c('BARC', 'CRAM', 'LIRO', 'PRLA', 'PRPO', 'SUGG', 'TOOK')
cols_modeltype <- c('empirical' = "#482576FF",
                    'multi-model ensemble' =  "#35608DFF",
                    'process' = "#21908CFF",
                    'ML' = '#BBDF27FF',
                    'null' = '#43BF71FF')
# read in the metadata from the google sheet
googlesheets4::gs4_deauth()
model_meta <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1oC7_w63wSCXNiHs1IK8AFGr0MG-NdjDAjwkfjvRZW-I/edit?usp=sharing")

# Open dataset-------------
# calculate chla skill scores
chla_scores <- arrow::open_dataset("scores") |>
  filter(site_id %in% lake_sites,
         # model_id != 'climatology',
         model_id %in% model_meta$model_id,
         variable == 'chla') |>
  collect()

# Calculate skill scores ------------
chla_persistence <- arrow::open_dataset("scores") |>
  filter(model_id == 'persistenceRW', 
         site_id %in% lake_sites,
         variable == 'chla') |>
  collect() |> 
  select(reference_datetime, datetime, crps, logs, site_id, model_id) |> 
  arrange(reference_datetime, datetime, site_id) |> 
  mutate(bias = observation - mean) |> 
  pivot_wider(values_from = c(crps, logs),
              names_from = model_id)

chla_skill <- chla_scores |>
  filter(model_id != 'persistenceRW') |> 
  select(reference_datetime, datetime, crps, logs, site_id, model_id) |> 
  full_join(chla_persistence) |> 
  
  # calcaulte the skill relative to climatology (- is bad, + is good)
  mutate(skill_crps = crps_persistenceRW - crps,
         skill_logs = logs_persistenceRW - logs,
         horizon = as.numeric(as_date(datetime) - as_date(reference_datetime))) |> 
  # consistent forecast period
  filter(horizon <= 30,
         horizon >= 1,
         model_id != 'fARIMA') |> 
  arrange(reference_datetime, site_id, datetime, model_id) 
#--------------------------------------#

chla_skill |> 
  group_by(model_id) |> 
  summarise(median_crps = median(skill_crps, na.rm = T)) |> 
  ungroup() |> 
  slice_max(median_crps, n= 10, na_rm = T) |> 
  left_join(model_meta) |> 
  mutate(model_id = fct_reorder(model_id, median_crps)) |>
  ggplot(aes(x=median_crps, 
             y = model_id,
             fill = model_type)) +
  geom_bar(stat = 'identity') +
  scale_y_reordered() +
  labs(y='Model ID', x = 'median CRPS skill (vs persistence)') +
  geom_vline(xintercept = 0) +
  scale_fill_manual(values = cols_modeltype) +
  theme_bw() 

chla_skill |> 
  group_by(model_id, site_id) |> 
  summarise(median_crps = median(skill_crps, na.rm = T)) |> 
  group_by(site_id) |> 
  slice_max(median_crps, n= 5, na_rm = T) |> 
  mutate(model_id = fct_reorder(model_id, desc(median_crps))) |>
  left_join(model_meta) |> 
  ggplot(aes(x=median_crps, 
             y = reorder_within(x = model_id, within = site_id, by = median_crps),
             fill = model_type)) +
  geom_bar(stat = 'identity') +
  scale_y_reordered() +
  facet_wrap(~site_id, scales = 'free') +
  labs(y='model_id', x = 'median CRPS skill (vs climatology)') +
  geom_vline(xintercept = 0)
