generate_forecast_score_arrow <- function(targets_file,
                                          forecast_df,
                                          use_s3 = FALSE,
                                          bucket = NULL,
                                          endpoint = NULL,
                                          local_directory = NULL){
  
  
  if(use_s3){
    if(is.null(bucket) | is.null(endpoint)){
      stop("scoring function needs bucket and endpoint if use_s3=TRUE")
    }
    vars <- FLAREr:::arrow_env_vars()
    output_directory <- arrow::s3_bucket(bucket = bucket,
                                         endpoint_override =  endpoint)
    FLAREr:::unset_arrow_vars(vars)
  }else{
    if(is.null(local_directory)){
      stop("scoring function needs local_directory if use_s3=FALSE")
    }
    output_directory <- arrow::SubTreeFileSystem$create(local_directory)
  }
  
  target <- readr::read_csv(targets_file, show_col_types = FALSE) 
  
  df <- forecast_df %>%
    score4cast::standardize_forecast() %>%
    dplyr::mutate(family = as.character(family)) |> 
    score4cast::crps_logs_score(target) %>%
    dplyr::mutate(horizon = datetime-lubridate::as_datetime(reference_datetime)) %>%
    dplyr::mutate(horizon = as.numeric(lubridate::as.duration(horizon),
                                       units = "seconds"),
                  horizon = horizon / 86400) %>%
    dplyr::mutate(depth = as.numeric(str_split_fixed(site_id, "-", 2)[,2]),
                  site_id = str_split_fixed(site_id, "-", 2)[,1])
  
  
  # reference_datetime_format <- "%Y-%m-%d %H:%M:%S"
  
  df <- df |> dplyr::mutate(reference_datetime = as_date(reference_datetime))
  
  arrow::write_dataset(df, path = output_directory, partitioning = c("model_id","site_id", "reference_datetime", "datetime"))
  
}