# Suite of function to compute epidemic risks

add_epi_risks <- function(epi_indicators, risk_types, risk_thresholds,
  n_reference_weeks, n_observation_weeks) {
  # Add column with computed risks and thresholds to the epidemic data.

  epi_risk_values <- NULL

  # Comput eand add risk thresholds.
  for (rty in risk_types) {

    if (rty == "absolute") {

      absolute_risk_thresholds <- risk_thresholds |>
        dplyr::filter(risk_type == rty)

    } else if (rty == "anomaly") {

      absolute_risk_thresholds <- get_abs_thresholds_anomaly(
        epi_indicators, rty, risk_thresholds, n_reference_weeks,
        n_observation_weeks
      )

    } else {

      stop("Unknown risk type: ", rty)

    }

    tmp_epi_risk_values <- epi_indicators |>
      dplyr::left_join(absolute_risk_thresholds)

    epi_risk_values <- epi_risk_values |>
      dplyr::bind_rows(tmp_epi_risk_values)

  }

  # Comput erisk levels by comparing indicator values and risk thresholds.
  epi_risk_values <- epi_risk_values |>
    dplyr::mutate(
      risk_level = dplyr::case_when(
        indicator_value < low_mid ~ "low",
        indicator_value >= low_mid & indicator_value < mid_high ~ "moderate",
        TRUE ~ "high"
      )
    )

  return(epi_risk_values)

}

get_abs_thresholds_anomaly <- function(epi_indicators, rty, risk_thresholds,
  n_reference_weeks, n_observation_weeks) {
  # Compute absolute thresholds from statistical modelling based on indicated
  # "anomaly thresholds" (the p-values on the estimated negative-binomial
  # distribution).

  districts_df <- epi_indicators |> dplyr::distinct(district)

  # Get weeks used to train the statistical model.
  weeks <- sort(unique(epi_indicators$week))
  train_weeks <- tail(
    weeks,
    n_reference_weeks + n_observation_weeks
  )[1:n_reference_weeks]

  absolute_risk_thresholds <- NULL

  # Compute risk thresholds for each disease and indicator.
  for (dis in unique(risk_thresholds$disease)) {
    for (ity in unique(risk_thresholds$indicator_type)) {

      rth <- risk_thresholds |>
        dplyr::filter(disease == dis & indicator_type == ity & risk_type == rty)

      train_data <- epi_indicators |>
        dplyr::filter(
          week %in% train_weeks &
          disease == dis &
          indicator_type == ity
        )

      # If there are less than two different indicator values, directly use
      # those as thresholds instead of trying to fit a model.
      if(length(unique(train_data$indicator_value)) > 2) {

        stats_model <- MASS::glm.nb(
          indicator_value ~ district,
          data = train_data
        )
        nb_mean <- exp(predict(stats_model, newdata = districts_df))
        low_mid_ <- stats::qnbinom(
          rth$low_mid, mu = nb_mean, size = stats_model$theta
        )
        attributes(low_mid_) <- NULL
        mid_high_ <- stats::qnbinom(
          rth$mid_high, mu = nb_mean, size = stats_model$theta
        )
        attributes(mid_high_) <- NULL

      } else {

        low_mid_ <- min(unique(train_data$indicator_value), na.rm = TRUE)
        mid_high_ <- max(unique(train_data$indicator_value), na.rm = TRUE)

      }

      abs_th <- tibble::tibble(
        disease = dis,
        district = districts_df$district,
        indicator_type = ity,
        risk_type = rty,
        low_mid = low_mid_,
        mid_high = mid_high_
      )

      absolute_risk_thresholds <- absolute_risk_thresholds |>
        dplyr::bind_rows(abs_th)

    }
  }

  return(absolute_risk_thresholds)

}
