score_weather_forecast <- function (forecast, target) {

  if ('prediction' %in% colnames(target)) {
    target <- rename(target, observation = prediction)
  }

  target <- dplyr::select(target,
                          "datetime",
                          "site_id",
                          "variable",
                          "observation")

  joined <- forecast |>
    mutate(family = ifelse(family == "ensemble","sample", family)) |>
    left_join(target,
              by = c("site_id", "datetime", "variable"))

  grouping <- c("model_id", "reference_datetime", "site_id",
                "datetime", "family", "variable")

  scores <- joined |>
    group_by(dplyr::across(dplyr::any_of(grouping))) |>
    summarise(observation = unique(observation),
              crps = score4cast:::generic_crps(family,
                                               parameter,
                                               prediction,
                                               observation),
              # logs = score4cast:::generic_logs(family,
              #                                  parameter,
              #                                  prediction,
              #                                  observation),
              dist = score4cast:::infer_dist(family,
                                             parameter,
                                             prediction),
              .groups = "drop") |>
    mutate(mean = mean(dist),
           median = stats::median(dist),
           sd = sqrt(distributional::variance(dist)),
           quantile97.5 = distributional::hilo(dist, 95)$upper,
           quantile02.5 = distributional::hilo(dist, 95)$lower,
           quantile90 = distributional::hilo(dist, 90)$upper,
           quantile10 = distributional::hilo(dist, 90)$lower,
           logs = NA) |>
    select(-dist)
  scores
}
