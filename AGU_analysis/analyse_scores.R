
library(tidyverse)
library(arrow)
library(lubridate)
library(ggh4x)

# read in the all submissions ensemble for the lake sites
lake_sites <- c('BARC', 'CRAM', 'LIRO', 'PRLA', 'PRPO', 'SUGG', 'TOOK')

all_ensemble_scores <- arrow::open_dataset("scores_rescored/") |>
  filter(site_id %in% lake_sites,
         variable == 'temperature') |>
  collect() 

top10 <- all_ensemble_scores |> 
  mutate(horizon = as_date(datetime) - as_date(reference_datetime)) |>
  group_by(model_id) |> 
  summarise(mean_crps = mean(crps, na.rm = T)) |> 
  ungroup() |> 
  slice_min(mean_crps, n= 10)

googlesheets4::gs4_deauth()
model_meta <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1oC7_w63wSCXNiHs1IK8AFGr0MG-NdjDAjwkfjvRZW-I/edit?usp=sharing")

all_ensemble_scores |> 
  mutate(horizon = as.numeric(as_date(datetime) - as_date(reference_datetime))) |>
  filter(between(horizon, 0, 30), model_id %in% top10$model_id) |> 
  group_by(horizon, model_id, site_id) |> 
  summarise(mean_crps = mean(crps, na.rm = T)) |> 
  ggplot(aes(x=horizon, y= mean_crps, colour = model_id)) +
  geom_line() +
  facet_wrap(~site_id)

design <- "
  A#
  BC
  DE
  FG
"

example_plot <- all_ensemble_scores |> 
  filter(reference_datetime == '2023-07-01') |> 
  mutate(horizon = as.numeric(as_date(datetime) - as_date(reference_datetime)),
         site_id = factor(site_id, levels = c('TOOK', 'PRLA', 'PRPO', 'CRAM', 'LIRO', 'SUGG', 'BARC'))) |>
  filter(between(horizon, 1, 30), 
         model_id %in% c('flareGLM', 'xgboost_parallel', 'tg_randfor', 'cb_prophet', 'tg_lasso')) |> 
  ggplot() +
  geom_point(aes(x=datetime, y=observation, shape = 'Observation')) +
  # geom_ribbon(aes(x=datetime, ymax=quantile97.5, ymin=quantile02.5, fill = model_id), alpha = 0.2)+
  geom_line(aes(x=datetime, y= mean, colour = model_id)) +
  facet_manual(vars(site_id), scales = 'free_y', design = design, labeller = label_both) +
  scale_shape_manual(values = 16, name = '') +
  scale_x_datetime(breaks = '10 days', date_labels = '%d %b') +
  theme_bw(base_size = 14)  +
  labs(x = 'Date', y='Temperature (°C)', colour = 'Model', title = 'Forecasts generated on:\nJuly 1, 2023') 

ggsave(example_plot, filename = './AGU_analysis/example_forecast.png', height = 19, width = 19, units = 'cm')

#----- rankings ------
n_forecasts <- all_ensemble_scores %>%
  mutate(horizon = as.numeric(as_date(datetime) - as_date(reference_datetime))) |>
  filter(horizon %in% c(1:30),
         !is.na(crps)) |>
  group_by(horizon, model_id, site_id) |> 
  summarise(n=n())


ranks <- all_ensemble_scores %>%
  mutate(horizon = as.numeric(as_date(datetime) - as_date(reference_datetime))) |>
  filter(horizon %in% c(1:30),
         !is.na(crps)) |>
  
  select(reference_datetime, horizon, site_id, model_id, crps) |>
  arrange(reference_datetime, horizon, site_id, crps) |>
  group_by(reference_datetime, horizon, site_id) |>
  mutate(rank = row_number()) |> 
  
  group_by(horizon, site_id, model_id, rank) |> 
  summarise(n_rank = n()) |> 
  full_join(n_forecasts) |> 
  mutate(proportion = n_rank/n) 


# if all models were submitted at all times and sites
all_vals <- expand.grid(rank = seq(1,38),
                        site_id = unique(ranks$site_id),
                        model_id = unique(ranks$model_id), 
                        horizon = unique(ranks$horizon))

ranks |> 
  full_join(all_vals) |> 
  mutate(proportion = ifelse(is.na(proportion), 0, proportion)) |> 
  filter(model_id %in% c('flareGLM', 'cb_prophet', 'fARIMA', 'tg_randfor', 'all_submissions', 'xgboost_parallel', 'tg_lasso')) |> 
  mutate(group_rank = cut(rank,
                          breaks = ceiling(seq(0, 38, length.out = 11)),
                          labels = seq(1,10, length.out = 10))) |> 
  group_by(group_rank, site_id, model_id, horizon) |> 
  summarise(proportion = sum(proportion)) |> 
  ggplot() +
  geom_area(aes(x=horizon, y=proportion, fill=fct_rev(as.factor(group_rank))),
            colour="black", stat = 'identity', position = 'stack') +
  facet_grid2(site_id~factor(model_id),
              axes = 'all', remove_labels = 'all') +
  scale_fill_brewer(palette = 'RdBu', name = 'Rank group') +
  theme_bw() +
  theme(panel.spacing.x = unit(1.1, 'lines'),
        panel.spacing.y = unit(1.1, 'lines')) +
  coord_cartesian(ylim = c(0,1), xlim = c(1,30)) +
  scale_y_continuous(expand = c(0,-0.10), name = 'Proportion of forecasts') +
  scale_x_continuous(expand = c(0,0), name = 'Horizon (days)')

#---------------------------------#

# rankings aggregated
n_forecasts <- all_ensemble_scores %>%
  mutate(horizon = as.numeric(as_date(datetime) - as_date(reference_datetime))) |>
  filter(horizon %in% c(1:30),
         !is.na(crps)) |>
  group_by(horizon, model_id, site_id) |> 
  summarise(n=n()) |> 
  group_by(model_id) |> 
  summarise(n = sum(n))


ranks <- all_ensemble_scores %>%
  mutate(horizon = as.numeric(as_date(datetime) - as_date(reference_datetime))) |>
  filter(horizon %in% c(1:30),
         !is.na(crps)) |>
  
  select(reference_datetime, horizon, site_id, model_id, crps) |>
  arrange(reference_datetime, horizon, site_id, crps) |>
  group_by(reference_datetime, horizon, site_id) |>
  mutate(rank = row_number()) |> 
  
  group_by(horizon, site_id, model_id, rank) |> 
  summarise(n_rank = n()) |>
  group_by(model_id, rank) |> 
  summarise(n_rank = sum(n_rank)) |> 
  full_join(n_forecasts) |> 
  mutate(proportion = n_rank/n) 


# if all models were submitted at all times and sites
all_vals <- expand.grid(rank = seq(1,31),
                        model_id = unique(ranks$model_id))


design <- "
  A##
  BCD
  EFG
"


ranks |> 
  full_join(all_vals) |> 
  mutate(proportion = ifelse(is.na(proportion), 0, proportion)) |> 
  filter(model_id %in% c('all_submissions', 'cb_prophet', 'flareGLM', 'xgboost_parallel', 
                         'air2waterSat_2', 'tg_ets', 'tg_tbats')) |> 
  mutate(model_id = factor(model_id, levels = c('all_submissions', 'cb_prophet', 'flareGLM', 'xgboost_parallel', 
                                      'air2waterSat_2', 'tg_ets', 'tg_tbats'))) |> 
  mutate(group_rank = cut(rank,
                          breaks = ceiling(seq(0, 31, length.out =11)),
                          labels = seq(1,10, length.out = 10))) |> 
  group_by(group_rank, model_id) |> 
  summarise(proportion = sum(proportion)) |> 
  na.omit() |> 
  ggplot() +
  geom_bar(aes(x=proportion, y=group_rank, fill=fct_rev(as.factor(group_rank))),
           colour="black", stat = 'identity', position = 'stack') +
  facet_manual(vars(model_id), design = design) +
  # facet_manual(vars(site_id), scales = 'free_y', design = design, labeller = label_both) +
  
  scale_fill_brewer(palette = 'RdBu', name = 'Rank group') +
  theme_bw(base_size = 18) +
  theme(panel.spacing.x = unit(1.1, 'lines'),
        panel.spacing.y = unit(1.1, 'lines'), 
        legend.position = 'none', panel.grid.major.y = element_blank()) +
  scale_x_continuous(name = '% of forecasts', breaks = seq(0, 0.3, 0.1), labels = seq(0, 30, 10)) +
  scale_y_discrete(breaks = c(1,10), labels = c('best 10 %', 'worst 10 %'), name = '', limits = rev)
 #------------------------------------------


