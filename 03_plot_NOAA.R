library(tidyverse)
library(arrow)
library(lubridate)

lake_sites <- c('BARC', 'CRAM', 'LIRO', 'PRLA', 'PRPO', 'SUGG', 'TOOK')

# Open dataset-------------
# start with lake temperatures
NOAA_scores <- arrow::open_dataset("scores") |>
  filter(site_id %in% lake_sites,
         model_id %in% c('NOAA_daily', 'NOAA_daily_obs')) |>
  collect()

NOAA_scores |> 
  mutate(horizon = as_date(datetime) - as_date(reference_datetime)) |> 
  group_by(model_id, site_id, horizon) |> 
  summarise(mean_crps = mean(crps, na.rm = T)) |> 
  filter(horizon <= 30) |> 
  ggplot(aes(x=horizon, y=mean_crps, colour = model_id)) +
  geom_line() +
  facet_wrap(~site_id) +
  theme_bw() +
  labs(y = 'CRPS score (oC)', title = 'NOAA air temperature forecast')
