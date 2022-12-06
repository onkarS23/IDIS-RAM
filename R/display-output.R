# Compute plots and tables.

prepare_display <- function(epi_risks, display_filter, n_reference_weeks,
  n_observation_weeks) {
  # Plots are based on the data filtered for the selected disease, indicator and
  # risk. Only weeks in the reference and observation time ranges are
  # considered.

  relevant_weeks <- sort(unique(epi_risks$week)) |>
    tail(n_reference_weeks + n_observation_weeks)

  epi_risks_display <- epi_risks |>
    dplyr::filter(
      disease == display_filter[["disease"]] &
      indicator_type == display_filter[["indicator_type"]] &
      risk_type == display_filter[["risk_type"]] &
      week %in% relevant_weeks
    ) |>
    dplyr::mutate(
      week = ISOweek::ISOweek2date(paste0(week, "-1"))
    )

  return(epi_risks_display)

}

plot_timeseries_risk <- function(epi_risks_display, risk_colors,
  ncol_timeseries) {
  # Plot for each district indicators as time series, color values in function
  # of risk level, indicate thresholds.

  plot_time_series <- epi_risks_display |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = week,
        y = indicator_value
      ),
      color = "black"
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(district),
      ncol = ncol_timeseries,
      scales = "free_y"
    ) +
    ggplot2::lims(y = c(0, NA)) +
    ggplot2::geom_line(ggplot2::aes(y = low_mid), linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = mid_high), linetype = "dashed") +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(fill = risk_level), shape = 21)

  plot_time_series <- plot_theming(plot_time_series, risk_colors)

  return(plot_time_series)

}

plot_heatmap_risk <- function(epi_risks_display, risk_colors,
  n_observation_weeks) {
  # Risk diagram as a heat map for all districts and the observation weeks.

  observation_weeks <- tail(
    sort(unique(epi_risks_display$week)),
    n_observation_weeks
  )

  plot_heat_map <- epi_risks_display |>
    dplyr::filter(week %in% observation_weeks) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = week,
        y = district,
        fill = risk_level
      )
    ) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::scale_y_discrete(limits = rev)

  plot_heat_map <- plot_theming(plot_heat_map, risk_colors)

  return(plot_heat_map)

}

plot_theming <- function(gg_pl, risk_colors) {
  # Theming common to different plots.

  gg_pl +
    ggplot2::scale_x_date(date_labels = "%Y-%m-%d") +
    ggplot2::scale_fill_manual(values = risk_colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )

}
