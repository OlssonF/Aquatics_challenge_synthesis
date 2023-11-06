
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
# start with lake temperatures
lake_scores <- arrow::open_dataset("scores") |>
  filter(site_id %in% lake_sites,
         # model_id != 'climatology',
         model_id %in% model_meta$model_id,
         variable == 'temperature') |>
  collect()

stream_scores <- arrow::open_dataset("scores") |>
  filter(!site_id %in% lake_sites,
         # model_id != 'climatology',
         model_id %in% model_meta$model_id,
         variable == 'temperature') |>
  collect()

# Calculate skill scores ------------
climatology_lake <- arrow::open_dataset("scores") |>
  filter(model_id == 'climatology', 
         site_id %in% lake_sites,
         variable == 'temperature') |>
  collect() |> 
  select(reference_datetime, datetime, crps, logs, site_id, model_id) |> 
  arrange(reference_datetime, datetime, site_id) |> 
  pivot_wider(values_from = c(crps, logs),
              names_from = model_id)

lake_skill_scores <- lake_scores |>
  filter(model_id != 'climatology') |> 
  select(reference_datetime, datetime, crps, logs, site_id, model_id) |> 
  full_join(climatology_lake) |> 
  
  # calcaulte the skill relative to climatology (- is bad, + is good)
  mutate(skill_crps = crps_climatology - crps,
         skill_logs = logs_climatology - logs,
         horizon = as.numeric(as_date(datetime) - as_date(reference_datetime))) |> 
  # consistent forecast period
  filter(horizon <= 30,
         horizon >= 1,
         model_id != 'fARIMA') |> 
  arrange(reference_datetime, site_id, datetime, model_id) 


# stream skill scores
climatology_stream <- arrow::open_dataset("scores") |>
  filter(model_id == 'climatology', 
         !site_id %in% lake_sites,
         variable == 'temperature') |>
  collect() |> 
  select(reference_datetime, datetime, crps, logs, site_id, model_id) |> 
  arrange(reference_datetime, datetime, site_id) |> 
  pivot_wider(values_from = c(crps, logs),
              names_from = model_id)

stream_skill_scores <- stream_scores |>
  filter(model_id != 'climatology') |> 
  select(reference_datetime, datetime, crps, logs, site_id, model_id) |> 
  full_join(climatology_stream) |> 
  
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

# Question - how does model type impact skill?

# what are the top 5 models per site
lake_skill_scores |> 
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

lake_skill_scores |> 
  group_by(model_id) |> 
  summarise(mean_crps = mean(skill_crps, na.rm = T)) |> 
  ungroup() |> 
  slice_max(mean_crps, n= 10, na_rm = T) |> 
  left_join(model_meta) |> 
  mutate(model_id = fct_reorder(model_id, mean_crps)) |>
  ggplot(aes(x=mean_crps, 
             y = model_id,
             fill = model_type)) +
  geom_bar(stat = 'identity') +
  scale_y_reordered() +
  labs(y='Model ID', x = 'mean CRPS skill (vs climatology)') +
  geom_vline(xintercept = 0) +
  scale_fill_manual(values = cols_modeltype) +
  theme_bw() 


stream_skill_scores |> 
  group_by(model_id) |> 
  summarise(mean_crps = mean(skill_crps, na.rm = T)) |> 
  ungroup() |> 
  slice_max(mean_crps, n= 10, na_rm = T) |> 
  left_join(model_meta) |> 
  mutate(model_id = fct_reorder(model_id, desc(mean_crps))) |>
  ggplot(aes(x=mean_crps, 
             y = model_id,
             fill = model_type)) +
  geom_bar(stat = 'identity') +
  scale_y_reordered() +
  labs(y='model_id', x = 'mean CRPS skill (vs climatology)') +
  geom_vline(xintercept = 0) +
  scale_fill_manual(values = cols_modeltype)

lake_skill_scores |> 
  filter(horizon %in% c(1,7,30)) |> 
  group_by(model_id, site_id, horizon) |> 
  summarise(mean_crps = mean(skill_crps, na.rm = T)) |> 
  group_by(site_id, horizon) |> 
  slice_max(mean_crps, n= 5, na_rm = T) |> 
  left_join(model_meta) |> 
  mutate(model_id = fct_reorder(model_id, mean_crps)) |>
  ggplot(aes(x=mean_crps, 
             y = model_id, 
             fill = model_type)) +
  geom_bar(stat = 'identity') +
  # scale_y_reordered() +
  facet_grid(horizon~site_id, scales = 'free_y') +
  labs(y='model_id', x = 'mean CRPS skill (vs climatology)') +
  geom_vline(xintercept = 0)

lake_scores |> 
  left_join(model_meta, by = 'model_id') |> 
  group_by(model_type, site_id) |> 
  summarise(mean_crps = mean(crps, na.rm = T)) |> 
  mutate(model_type = fct_reorder(model_type, desc(mean_crps))) |>
  ggplot(aes(x=mean_crps, y = model_type)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~site_id)

lake_scores |> 
  left_join(model_meta, by = 'model_id') |> 
  group_by(model_type) |> 
  summarise(mean_crps = mean(crps, na.rm = T)) |> 
  mutate(model_type = fct_reorder(model_type, desc(mean_crps))) |>
  ggplot(aes(x=mean_crps, y = model_type, fill = model_type)) +
  geom_bar(stat = 'identity') 

# top 10 average models
top_mods <- lake_skill_scores |> 
  group_by(model_id) |> 
  summarise(mean_crps = mean(skill_crps, na.rm = T)) |> 
  ungroup() |> 
  slice_max(mean_crps, n = 10, na_rm = T) |> 
  distinct(model_id)

lake_skill_scores |> 
  filter(model_id %in% top_mods$model_id) |> 
  group_by(site_id) |> 
  summarise(mean_crps = mean(skill_crps, na.rm = T)) |> 
  mutate(site_id = fct_reorder(site_id, mean_crps)) |>
  ggplot(aes(x=mean_crps, 
             y = site_id)) +
  geom_bar(stat = 'identity') +
  scale_y_reordered() +
  labs(y='site_id', x = 'mean CRPS skill (vs climatology)') +
  geom_vline(xintercept = 0) +
  theme_bw()



lake_skill_scores |> 
  filter(model_id %in% top_mods$model_id) |> 
  group_by(site_id, horizon) |> 
  summarise(mean_crps = mean(skill_crps, na.rm = T)) |> 
  ggplot(aes(x=horizon, 
             y = mean_crps, colour = site_id)) +
  geom_hline(yintercept = 0) + 
  geom_line()  +  
  scale_colour_manual(values = cols_site, name = '') +
  labs(y='mean CRPS skill (vs climatology)', x = 'horizon (days)') +
  theme_bw()


lake_skill_scores |> 
  filter(model_id %in% top_mods$model_id,
         site_id == 'SUGG') |> 
  group_by(model_id, horizon, site_id) |> 
  summarise(mean_crps = mean(skill_crps, na.rm = T)) |> 
  ggplot(aes(x=horizon, 
             y = mean_crps, colour = model_id)) +
  geom_hline(yintercept = 0) + 
  geom_line()  +  
  # scale_colour_manual(values = cols_site, name = '') +
  labs(y='mean CRPS skill (vs climatology)', x = 'horizon (days)') +
  theme_bw() +facet_wrap(~site_id)
# 
climatology_scores |> 
  group_by(site_id) |> 
  summarise(mean_crps = mean(crps_climatology, na.rm = T)) |> 
  ggplot(aes(x=mean_crps, y=site_id))+
  geom_bar(stat ='identity')



