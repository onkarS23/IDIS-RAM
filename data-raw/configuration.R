# File containing the epidemic data in a format that can be loaded in SORMAS
epi_data_path <- "data"
epi_data_file_name <- "sormas_cases.csv"

# Date variables used for each indicator
week_types <- list(
  `case count` = "reporting_week",
  `death count` = "death_week",
  `hospitalization count` = "hospitalization_week"
)

# Which values of the field case.Classification to consider when filtering the
# raw data. If `NULL` no filtering is applied.
case_definitions <- NULL

# Diseases, indicators and risks considered.
diseases <- c("Coronavirus")
indicator_types <- c("case count", "death count", "hospitalization count")
risk_types <- c("absolute", "anomaly")

# Default values for the risk thresholds.
risk_thresholds_default <- tibble::tribble(
  ~risk_type, ~low_mid, ~mid_high,
  "absolute", 1,        10,
  "anomaly",  0.7,      0.9
)

# Default values for the number of reference and observation weeks. The first
# are those on which the statistical model is trained, the latter those the
# users are interested in.
n_reference_weeks_default <- 20
n_observation_weeks_default <- 10

# Default diseases, indicator and risk displayed.
display_filter_default <- list(
  disease = "Coronavirus",
  indicator_type = "case count",
  risk_type = "absolute"
)

# Color coding for risk levels.
risk_colors <- list(
  low = "#440154",
  moderate = "#21908c",
  high = "#fde725"
)

# Layout parameters.
sidebar_width <- 300
relative_height_heatmap <- 10
relative_height_timeseries <- 100
ncol_timeseries <- 4
