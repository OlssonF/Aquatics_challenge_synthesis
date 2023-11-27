knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(arrow)
library(lubridate)

# read in the all submissions ensemble for the lake sites
lake_sites <- c('BARC', 'CRAM', 'LIRO', 'PRLA', 'PRPO', 'SUGG', 'TOOK')

all_ensemble_scores <- arrow::open_dataset("scores_rescored") |>
  filter(site_id %in% lake_sites,
         model_id == 'all_submissions',
         variable == 'temperature') |>
  collect()

all_ensemble_scores |> 
  mutate(horizon = as_date(datetime) - as_date(reference_datetime)) |> 
  group_by(horizon, site_id, model_id) |> 
  summarise(mean_crps = mean(crps, na.rm = T)) |> 
  ggplot(aes(x=horizon, y= mean_crps)) +
  geom_line(aes(colour = model_id)) +
  facet_wrap(~site_id)
