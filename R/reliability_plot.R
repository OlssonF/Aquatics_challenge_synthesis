plot_reliability <- function(scores, models, CI, order = levels(models), alpha = 0.8) {
  
  if (CI == 95) {
    plot <- scores |> 
      inner_join(models, by = 'model_id') |> 
      mutate(horizon = as.numeric(as_date(datetime) - as_date(reference_datetime))) |> 
      filter(horizon >0,
             horizon != 15, 
             horizon <=30,
             !is.na(observation)) |> 
      mutate(is_in = between(observation, quantile02.5, quantile97.5)) |> 
      group_by(horizon, is_in, model_id, manuscript_name) |> 
      summarise(n = n()) |> 
      pivot_wider(names_from = is_in, names_prefix = 'within_', values_from = n, values_fill = 0) |> 
      mutate(perc = within_TRUE/(within_FALSE + within_TRUE)*100, 
             manuscript_name = factor(manuscript_name, levels = order)) |> 
      ggplot(aes(x=horizon, y=perc, colour = manuscript_name, linetype = manuscript_name)) +
      geom_hline(yintercept = 95, colour = 'grey3', linetype = 'dashed', linewidth = 0.8)+
      geom_line(linewidth = 0.8, alpha = alpha) +
      labs(y = 'Percentage of observations within\n95% confidence intervals', x='Horizon (days)')  +
      annotate('text', x = 30, y = 100, label = 'underconfident', size = 4, hjust = 1) +
      annotate('text', x = 30, y = 75, label = 'overconfident', size = 4, hjust = 1)
  }
  if (CI == 80) {
    plot <- scores |> 
      inner_join(models, by = 'model_id') |> 
      mutate(horizon = as.numeric(as_date(datetime) - as_date(reference_datetime))) |> 
      filter(horizon >0,
             horizon != 15, 
             horizon <=30,
             !is.na(observation)) |> 
      mutate(is_in = between(observation, quantile10, quantile90)) |> 
      group_by(horizon, is_in, model_id, manuscript_name) |> 
      summarise(n = n()) |> 
      pivot_wider(names_from = is_in, names_prefix = 'within_', values_from = n, values_fill = 0) |> 
      mutate(perc = within_TRUE/(within_FALSE + within_TRUE)*100, 
             manuscript_name = factor(manuscript_name, levels = order)) |> 
      ggplot(aes(x=horizon, y=perc, colour = manuscript_name, linetype = manuscript_name)) +
      geom_hline(yintercept = 80, colour = 'grey3', linetype = 'dashed', linewidth = 0.8)+
      geom_line(linewidth = 0.8, alpha = alpha) +
      labs(y = 'Percentage of observations within\n80% confidence intervals', x='Horizon (days)') +
      annotate('text', x = 30, y = 100, label = 'underconfident', size = 4, hjust = 1) +
      annotate('text', x = 30, y = 75, label = 'overconfident', size = 4, hjust = 1)
  }
  if (CI != 80 & CI != 95) {
    message('CI must be 80 or 95')
  } else {
    return(plot)
  }
}