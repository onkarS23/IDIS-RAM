# Minimal webapp to compute and visualize epidemic risks based on
# SOMRAS-compatible data.

source("data-raw/configuration.R")
source("R/data-preparation.R")
source("R/risk-computation.R")
source("R/display-output.R")

ui <- shinydashboard::dashboardPage(

  # Header

  shinydashboard::dashboardHeader(
    title = "IDIS RAM sketch",
    disable = F,
    titleWidth = 450,
    tags$li(
      class = 'dropdown',
      conditionalPanel(
        condition = '$("html").hasClass("shiny-busy")',
        'loading...'
      )
    )
  ),

  # Sidebar with controllers

  shinydashboard::dashboardSidebar(

    selectInput(
      "disease",
      "Disease",
      choices = diseases,
      selected = display_filter_default[["disease"]]
    ),

    numericInput(
      "n_reference_weeks",
      "Number of reference weeks",
      value = n_reference_weeks_default,
      min = 1
    ),

    numericInput(
      "n_observation_weeks",
      "Number of observation weeks",
      value = n_observation_weeks_default,
      min = 1
    ),

    selectInput(
      "indicator_type",
      "Indicator type",
      choices = indicator_types,
      selected = display_filter_default[["indicator_type"]]
    ),

    selectInput(
      "risk_type",
      "Risk type",
      choices = risk_types,
      selected = display_filter_default[["risk_type"]]
    ),

    numericInput(
      "low_mid_absolute",
      "Absolute low-moderate threshold",
      value = risk_thresholds_default$low_mid[
        risk_thresholds_default$risk_type == "absolute"
      ],
      min = 0
    ),

    numericInput(
      "mid_high_absolute",
      "Absolute moderate-high threshold",
      value = risk_thresholds_default$mid_high[
        risk_thresholds_default$risk_type == "absolute"
      ],
      min = 0
    ),

    sliderInput(
      "low_mid_anomaly",
      "Anomaly low-moderate threshold",
      value = risk_thresholds_default$low_mid[
        risk_thresholds_default$risk_type == "anomaly"
      ],
      min = 0,
      max = 1
    ),

    sliderInput(
      "mid_high_anomaly",
      "Anomaly moderate-high threshold",
      value = risk_thresholds_default$mid_high[
        risk_thresholds_default$risk_type == "anomaly"
      ],
      min = 0,
      max = 1
    ),

    width = sidebar_width
  ),

  # Main panel with outputs in different tabs

  shinydashboard::dashboardBody(
    fluidPage(
      tabsetPanel(

        type = "pills",

        tabPanel(
          "Risk",
          shinycssloaders::withSpinner(
            uiOutput("plot_heatmap_ui"),
            color='lightgrey'
          )
        ),

        tabPanel(
          "Time series",
          shinycssloaders::withSpinner(
            uiOutput("plot_timeseries_ui"),
            color='lightgrey'
          )
        ),

        tabPanel(
          "Data",
          shinycssloaders::withSpinner(
            DT::dataTableOutput("epi_risks"),
            color='lightgrey'
          )
        )

      )
    )
  )
)

server <- function(input, output, session) {

  # Parameters

  display_filter <- reactive({
    list(
      disease = input$disease,
      indicator_type = input$indicator_type,
      risk_type = input$risk_type
    )
  })

  risk_thresholds <- reactive({

    tibble::tibble(
      disease = input$disease,
      indicator_type = input$indicator_type,
      risk_type = risk_types,
      low_mid = c(input$low_mid_absolute, input$low_mid_anomaly),
      mid_high = c(input$mid_high_absolute, input$mid_high_anomaly)
    )

  })

  # Data

  epi_data_raw <- tibble::as_tibble(
    read.csv2(
      paste0(epi_data_path, "/", epi_data_file_name),
      skip = 1
    )
  )

  epi_indicators <- prepare_epi_data(epi_data_raw, indicator_types, week_types,
    case_definitions)

  epi_risks <- reactive({
    add_epi_risks(epi_indicators, risk_types, risk_thresholds(),
      input$n_reference_weeks, input$n_observation_weeks)
  })

  # Plots and tables

  epi_risks_display <- reactive({
    prepare_display(epi_risks(), display_filter(),
      input$n_reference_weeks, input$n_observation_weeks)
  })

  ## Heatmap
  heatmap_height <- reactive({
    round(relative_height_heatmap *
        length(unique(epi_risks_display()$district)))
  })
  output$plot_heatmap <- renderPlot({
    plot_heatmap_risk(epi_risks_display(), risk_colors,
      input$n_observation_weeks)
  })
  output$plot_heatmap_ui <- renderUI({
    plotOutput("plot_heatmap", height = heatmap_height())
  })

  ## Time series
  timeseries_height <- reactive({
    round(
      relative_height_timeseries *
      length(unique(epi_risks_display()$district)) /
      ncol_timeseries
    )
  })
  output$plot_timeseries <- renderPlot({
    plot_timeseries_risk(epi_risks_display(), risk_colors, ncol_timeseries)
  })
  output$plot_timeseries_ui <- renderUI({
    plotOutput("plot_timeseries", height = timeseries_height())
  })

  ## Table
  output$epi_risks <- DT::renderDataTable({
    DT::datatable(
      epi_risks(),
      style = "bootstrap",
      extensions = "Scroller",
      options = list(dom = "Bftlrip", scrollX = TRUE,
        buttons = c("csv", "excel"), rownames = FALSE, deferRender = TRUE,
        scrollY = 400, scroller = TRUE)
    ) |>
      DT::formatStyle(columns = 1:ncol(epi_risks()),`font-size` = "12px")
  })

}

shiny::shinyApp(ui, server)
