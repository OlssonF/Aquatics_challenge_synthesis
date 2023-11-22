
get_forecast  <- function(theme, 
                          model_id, 
                          forecast_date, 
                          var, 
                          h = 30) {
  s3_model <- s3_bucket(file.path('neon4cast-forecasts/parquet', theme, 
                                  paste0('model_id=', model_id), 
                                  paste0('reference_datetime=', forecast_date)),
                        endpoint_override= "data.ecoforecast.org")
  
  forecast <- arrow::open_dataset(s3_model) |>
    collect() |> 
    filter(variable == var, 
           datetime > forecast_date) |>
    group_by(site_id) |> 
    # remove sites that contain NAs
    filter(!any(is.na(prediction))) |> 
    ungroup() |> 
    mutate(model_id = model_id,
           horizon = as_date(datetime) - as_date(forecast_date)) |> 
    filter(horizon <= h) |> 
    select(-horizon)
  
  if (nrow(forecast) == 0) {
    message('No forecast for ', var, ' using ', model_id)
    return(forecast)
  } else {
    # do some more data wrangling
    message(model_id, ' read in')
    
    if (forecast$family[1] != 'sample') {
      message('generate ensemble of 100')
      
      forecast <- forecast |> 
        select(datetime, site_id, variable, family, parameter, prediction, model_id) |> 
        pivot_wider(names_from = parameter,
                    values_from = prediction, 
                    id_cols = c(datetime, site_id, model_id)) |> 
        
        group_by(site_id, datetime, model_id) |> 
        # sample from the distribution based on the mean and sd
        reframe(prediction = rnorm(100, mean = mu, sd = sigma)) |> 
        group_by(site_id, datetime) |> 
        # parameter value needs to be character
        mutate(parameter = as.character(row_number()),
               # model_id = ensemble_name, 
               reference_datetime = forecast_date,
               variable = var,
               family = 'ensemble')
    }
  }     
  return(forecast)

}