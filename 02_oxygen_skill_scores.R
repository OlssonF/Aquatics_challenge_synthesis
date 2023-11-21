
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
# calculate DO skill scores
DO_scores <- arrow::open_dataset("scores") |>
  filter(site_id %in% lake_sites,
         # model_id != 'climatology',
         model_id %in% model_meta$model_id,
         variable == 'oxygen') |>
  collect()

# Calculate skill scores ------------
DO_climatology <- arrow::open_dataset("scores") |>
  filter(model_id == 'climatology', 
         site_id %in% lake_sites,
         variable == 'oxygen') |>
  collect() |> 
  select(reference_datetime, datetime, crps, logs, site_id, model_id) |> 
  arrange(reference_datetime, datetime, site_id) |> 
  pivot_wider(values_from = c(crps, logs),
              names_from = model_id)

DO_skill <- DO_scores |>
  filter(model_id != 'climatology') |> 
  select(reference_datetime, datetime, crps, logs, site_id, model_id) |> 
  full_join(DO_climatology) |> 
  
  # calcaulte the skill relative to climatology (- is bad, + is good)
  mutate(skill_crps = crps_climatology - crps,
         skill_logs = logs_climatology - logs,
         horizon = as.numeric(as_date(datetime) - as_date(reference_datetime))) |> 
  # consistent forecast period
  filter(horizon <= 30,
         horizon >= 1,
         model_id != 'fARIMA') |> 
  arrange(reference_datetime, site_id, datetime, model_id) 
#------------------------------------#

DO_skill |> 
  group_by(model_id, site_id) |> 
  summarise(mean_crps = mean(skill_crps, na.rm = T)) |> 
  group_by(site_id) |> 
  slice_max(mean_crps, n= 5, na_rm = T) |> 
  mutate(model_id = fct_reorder(model_id, desc(mean_crps))) |>
  left_join(model_meta) |> 
  ggplot(aes(x=mean_crps, 
             y = reorder_within(x = model_id, within = site_id, by = mean_crps),
             fill = model_type)) +
  geom_bar(stat = 'identity') +
  scale_y_reordered() +
  facet_wrap(~site_id, scales = 'free_y') +
  labs(y='model_id', x = 'mean CRPS skill (vs climatology)') +
  geom_vline(xintercept = 0)
