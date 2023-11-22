create_all_mme <- function(forecast_models = 'all', # vector of list of model names
                           n = 200, # what size ensemble do you want?
                           ensemble_name, # what is the name of the ensemble output
                           forecast_date, # when is the forecast for (what forecast to grab)
                           var, # which variable do you want a forecast for
                           h = 30, # what is the required forecast horizon
                           theme, # challenge theme 
                           omit = NA,  # do you want to omit any?
                           out_dir, # where to save
                           out_format = 'csv') # file type
  {
  s3_theme <- s3_bucket(paste0('neon4cast-forecasts/parquet/', theme),
                        endpoint_override= "data.ecoforecast.org")
  
  mods <- data.frame(model_id = s3_theme$ls(),
                     present = NA) |> 
    filter(!model_id %in% omit)
  
  message('Checking which models exist for this forecast date')
  
  for (i in 1:nrow(mods)) {
    
    s3_model <- s3_bucket(paste0('neon4cast-forecasts/parquet/', theme, '/',
                                 mods$model_id[i]),
                          endpoint_override= "data.ecoforecast.org")
    
    output <- ifelse(length(which(s3_model$ls() == paste0('reference_datetime=',forecast_date))) == 1, T, F)
    
    mods$present[i] <- output
    
  }
  
  # which models are present for this forecast date?
  mods_present <- mods |> 
    filter(present == T)
  
  # get all the forecasts that are present for this date and variable.
  # function will generate an ensemble if it's a normal distribution
  forecasts <- map_dfr(
    .x = gsub(mods_present$model_id, pattern=  'model_id=', replacement = ''), 
    .f = ~ get_forecast(model_id = .x, theme = theme, forecast_date = forecast_date))
  
  # How many from each forecast should be sampled
  n_models <- distinct(forecasts, model_id, site_id) |> 
    group_by(site_id) |> 
    summarise(n_models = n()) |> 
    mutate(sample = round(n / n_models, digits = 0))

  # split into a list of dataframes
  sitewise_mme <- split(forecasts, f = forecasts$site_id)

  mme_forecast <- NULL
  
  # sub-sample at each site
  for (i in 1:nrow(n_models)) {
    mme <- sitewise_mme[[n_models$site_id[i]]]
    
    sample <- n_models$sample[i]
    
    # sub-sample the ensemble members
    mme_sample <- mme %>%
      group_by(model_id) |> 
      distinct(parameter) %>%
      slice_sample(n = sample) %>%
      ungroup() |> 
      left_join(mme, by = c("parameter", 'model_id'), multiple = 'all') %>%
      mutate(reference_datetime = forecast_date,
             parameter = as.character(parameter)) 
    
    # need to recode the parameter values so each is unqiue
    mme_recode <- mme_sample |> 
      group_by(datetime) |> 
      mutate(parameter = row_number(),
             family = 'ensemble') |> 
      ungroup() |> 
      mutate(model_id = ensemble_name) |> 
      filter(datetime > forecast_date)
    
    mme_forecast <- bind_rows(mme_forecast, mme_recode)
    
    
  }
  

  if (out_format == 'csv') {
    filename <- paste0(theme, '-', forecast_date, '-', ensemble_name, '.csv.gz')
    
    mme_forecast |>
      select(-any_of(c('pubDate', 'date'))) |> 
      readr::write_csv(file.path(out_dir, filename))
    
    message(ensemble_name, ' generated')
    
    valid <- neon4cast::forecast_output_validator(file.path(out_dir, filename))
    
    if (!valid) {
      file.remove(file.path('./Forecasts/ensembles', filename))
      message('forecast not valid')
    } else {
      return(filename)
    }
  }
  
  if (out_format == 'parquet') {
    mme_forecast |>
      select(-any_of(c('pubDate'))) |> 
      mutate(date = as_date(datetime)) |> 
      group_by(model_id, reference_datetime, date) |> 
      arrow::write_parquet(out_dir)
  }
  
  
  
}

