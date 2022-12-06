# From raw data prepare epidemic count-based indicators, filling in with 0's
# for factor combinations not present in the data.

prepare_epi_data <- function(epi_data_raw, indicator_types, week_types,
  case_definitions) {
  # Format the raw daat and add computed indicators.

  epi_indicators <- epi_data_raw |>
    format_epi_data() |>
    add_indicators(indicator_types, week_types, case_definitions)

 return(epi_indicators)

}


format_epi_data <- function(epi_data_raw) {
  # Select relevant variables, rename them for convenience, convert dates in
  # proper date formats and then cast them in weeks.

  epi_data <- epi_data_raw |>
    dplyr::select(disease, reportDate, person.address.city, caseClassification,
      person.deathDate, hospitalization.admissionDate) |>
    dplyr::rename(
      reporting_date = reportDate,
      district = person.address.city,
      case_classification = caseClassification,
      death_date = person.deathDate,
      hospitalization_date = hospitalization.admissionDate
    ) |>
    dplyr::mutate(
      reporting_date = lubridate::mdy(reporting_date),
      death_date = lubridate::mdy(death_date),
      hospitalization_date = lubridate::mdy(hospitalization_date),
      reporting_week = ISOweek::ISOweek(reporting_date),
      death_week = ISOweek::ISOweek(death_date),
      hospitalization_week = ISOweek::ISOweek(hospitalization_date),
    ) |>
    dplyr::select(-reporting_date, -death_date, -hospitalization_date)

  return(epi_data)

}

get_factor_combinations <- function(epi_data) {
  # Get all possible factor combinations.

  factors <- list(
    disease = sort(unique(epi_data$disease)),
    district = sort(unique(epi_data$district)),
    week = get_all_weeks(
      c(epi_data$reporting_week, epi_data$death_week,
        epi_data$hospitalization_week)
    )
  )

  combinations <- purrr::cross_df(factors)

  return(combinations)

}

get_all_weeks <- function(weeks) {
  # Get all weeks between first and last `weeks`.

  weeks <- weeks[!is.na(weeks)]

  all_weeks <- ISOweek::ISOweek(
    sort(unique(seq(
      ISOweek::ISOweek2date(min(paste0(weeks, "-1"), na.rm = TRUE)),
      ISOweek::ISOweek2date(max(paste0(weeks, "-1"), na.rm = TRUE)),
      by = "week"
    )))
  )

  return(all_weeks)

}

add_indicators <- function(epi_data, indicator_types, week_types,
  case_definitions) {
  # Compute indicators: case count, death count and hospitalization count.

  factor_combinations <- get_factor_combinations(epi_data)

  epi_indicators <- NULL

  for (ity in indicator_types) {

    indicator <- epi_data

    # Filter case definition.
    if (!is.null(case_definitions)) {
      indicator <- indicator |>
      dplyr::filter(case_classification %in% case_definitions)
    }
    indicator <- indicator |>
      dplyr::select(-case_classification)

    # Use the generic variable name `week` instead of indicator-sepicifc names.
    indicator$week <- indicator[[week_types[[ity]]]]

    for (ity1 in indicator_types) {
      indicator[[week_types[[ity1]]]] <- NULL
    }

    if (ity %in%
        c("case count", "death count", "hospitalization count")) {

      indicator <- indicator |> dplyr::filter(!is.na(week))

    } else {

      stop("Unknown indicator type: ", ity)

    }

    # Count occurrences of combinations, and  count 0 for combinations not in
    # the raw data.
    indicator <- indicator |>
      dplyr::count(across(), name = "indicator_value") |>
      dplyr::right_join(
        factor_combinations
      ) |>
      dplyr::mutate(
        indicator_value = dplyr::if_else(
          is.na(indicator_value), 0L, indicator_value
        )
      ) |>
      dplyr::mutate(indicator_type = ity)

    epi_indicators <- epi_indicators |> dplyr::bind_rows(indicator)

  }

  return(epi_indicators)

}
