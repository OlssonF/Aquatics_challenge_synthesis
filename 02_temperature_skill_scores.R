
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
temperature_scores <- arrow::open_dataset("scores") |>
  filter(site_id %in% lake_sites,
         # model_id != 'climatology',
         model_id %in% model_meta$model_id,
         variable == 'temperature') |>
  collect()


# Calculate skill scores ------------
temperature_climatology <- arrow::open_dataset("scores") |>
  filter(model_id == 'climatology', 
         site_id %in% lake_sites,
         variable == 'temperature') |>
  collect() |> 
  select(reference_datetime, datetime, crps, logs, site_id, model_id) |> 
  arrange(reference_datetime, datetime, site_id) |> 
  pivot_wider(values_from = c(crps, logs),
              names_from = model_id)

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
         model_id != 'fARIMA') |> 
  arrange(reference_datetime, site_id, datetime, model_id) 

# --- Question - how does model type impact skill? ----

# what are the top 5 models per site
temperature_skill |> 
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
  scale_fill_manual(values = cols_modeltype) +
  facet_wrap(~site_id, scales = 'free_y',nrow = 4) +
  labs(y='model_id', x = 'mean CRPS skill (vs climatology)') +
  geom_vline(xintercept = 0) +
  theme_bw()


temperature_skill |> 
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

temperature_scores |> 
  left_join(model_meta, by = 'model_id') |> 
  group_by(model_type, site_id) |> 
  summarise(mean_crps = mean(crps, na.rm = T), 
            stderror = std_error(crps),
            sd = sd(crps, na.rm = T)) |> 
  group_by(site_id) |> 
  mutate(min = ifelse(mean_crps == min(mean_crps), T, F)) |> 
  ggplot(aes(x=mean_crps, 
             y = model_type,
             fill = min)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(xmin = mean_crps - sd, 
                    xmax = mean_crps + sd), alpha = 0.5) +
  scale_y_reordered() +
  facet_wrap(~site_id, scales = 'free_y',nrow = 4) +
  labs(y='model_id', x = 'mean CRPS score') +
  theme_bw()

temperature_scores |> 
  left_join(model_meta, by = 'model_id') |> 
  group_by(model_type, model_id, site_id) |> 
  summarise(mean_crps = mean(crps, na.rm = T)) |>  
  #           stderror = std_error(crps),
  #           sd = sd(crps, na.rm = T)) |> 
  # group_by(site_id) |> 
  # mutate(min = ifelse(mean_crps == min(mean_crps), T, F)) |> 
  ggplot(aes(x = mean_crps, 
             y = site_id)) +
  geom_boxplot() +
  facet_wrap(~model_type, scales = 'free_y',nrow = 4) +
  labs(y='model_id', x = 'mean CRPS score') +
  theme_bw()

temperature_scores |> 
  left_join(model_meta, by = 'model_id') |> 
  group_by(model_type) |> 
  summarise(mean_crps = mean(crps, na.rm = T)) |> 
  mutate(model_type = fct_reorder(model_type, desc(mean_crps))) |>
  ggplot(aes(x=mean_crps, y = model_type, fill = model_type)) +
  geom_bar(stat = 'identity') 

# top 10 average models
top_mods <- temperature_skill |> 
  group_by(model_id) |> 
  summarise(mean_crps = mean(skill_crps, na.rm = T)) |> 
  ungroup() |> 
  slice_max(mean_crps, n = 10, na_rm = T) |> 
  distinct(model_id)

temperature_skill |> 
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



temperature_skill |> 
  filter(model_id %in% top_mods$model_id) |> 
  group_by(site_id, horizon) |> 
  summarise(mean_crps = mean(skill_crps, na.rm = T)) |> 
  ggplot(aes(x=horizon, 
             y = mean_crps, colour = site_id)) +
  geom_hline(yintercept = 0) + 
  geom_line()  +  
  labs(y='mean CRPS skill (vs climatology)', x = 'horizon (days)') +
  theme_bw()




#------------------------------------------------------#

# --- Question - how does co-variate inclusion impact skill? ----
temperature_scores |>
  left_join(model_meta, by = 'model_id') |> 
  group_by(uses_NOAA, site_id) |> 
  summarise(mean_crps = mean(crps, na.rm = T), 
            stderror = std_error(crps),
            sd = sd(crps, na.rm = T)) |> 
  ggplot(aes(x = site_id, 
             y = mean_crps,
             fill = as_factor(uses_NOAA))) +
  geom_bar(stat = 'identity', 
           position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_crps - sd, 
                    ymax = mean_crps + sd), 
                alpha = 0.5, 
                position = position_dodge(0.9),
                width =.2) +
  scale_y_continuous(expand = c(0,0))
  theme_bw()



