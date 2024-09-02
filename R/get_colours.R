# top 10 average models
top_mods <- temperature_skill |> 
  group_by(model_id) |> 
  summarise(mean_crps = mean(skill_crps, na.rm = T)) |> 
  ungroup() |> 
  filter(mean_crps > 0) |> 
  left_join(select(model_meta, model_id, manuscript_name, model_type)) |> 
  arrange(model_type) |> 
  # also include the baseline models
  full_join(select(baselines, model_id, manuscript_name, model_type)) 


# define the colours
top_process <- top_mods |> filter(model_type == 'process-based') |> pull(model_id)
process_cols <- RColorBrewer::brewer.pal(n = 9, name = 'YlGnBu')[3:6]
names(process_cols) <- top_process

top_MME <- top_mods |> filter(model_type == 'MME') |> pull(model_id)
MME_cols <- RColorBrewer::brewer.pal(n = 9, name = 'YlOrRd')[c(4,5)]
names(MME_cols) <- top_MME


top_empirical <- top_mods |> filter(model_type == 'empirical') |> pull(model_id)
empirical_cols <- RColorBrewer::brewer.pal(n = 9, name = 'Reds')[6]
names(empirical_cols) <- top_empirical


top_ML <- top_mods |> filter(model_type == 'ML') |> pull(model_id)
ML_cols <- RColorBrewer::brewer.pal(n = 6, name = 'Purples')[c(2,4,6)]
names(ML_cols) <- top_ML

# make into a dataframe
model_id_colours <- enframe(c(process_cols, MME_cols, empirical_cols, ML_cols)) |> 
  full_join(top_mods, by = join_by(name == model_id)) |> 
  mutate(value = ifelse(is.na(value), '#A9A9A9', value),
         mean_crps = ifelse(is.na(mean_crps), 0, mean_crps),
         manuscript_name = fct_reorder(manuscript_name, mean_crps, .desc = T)) |> 
  arrange(desc(mean_crps)) %>% 
  mutate(linetype = ifelse(name == 'climatology', 'dotdash', 'solid'))

# colours for model types 
cols_modeltype <- c('empirical' = "#C42503FF" ,
                    'MME' = "#FDAD35FF"  ,
                    'process-based' = "#23C3E4FF" ,
                    'ML' = "#3E378FFF",
                    'null' = '#A9A9A9')