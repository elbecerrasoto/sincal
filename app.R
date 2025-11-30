library(shiny)
library(tidyverse)
library(glue)
library(bs4Dash)
# library(DT)
# library(shinyFeedback)
# library(openxlsx)
# library(tools)

# Load your helper functions
source("app_helper.R")

# globals ----

MIP_PATH <- "data/mip_sinaloa.tsv"
EMPLOYMENT_PATH <- "data/empleos_impuestos.tsv"
TEMPLATE_PATH <- "data/input_scheme.tsv"
ORI_DEST_PATH <- "data/origen_destino.rds"

MXN_USD <- 18.5 # Exchange rate MXN to USD
ROUND <- 2 # Rounding of numbers for printing
MIP_SCALE <- 1e6 # The scale input of the MIP, in this case millions of MXN

# Leontief Stuff
# Note: get_ZAB_LG_fx_Madds is likely in helper.R (sourced by app_helper.R)
SINALOA <- read_tsv(MIP_PATH) |>
  get_ZAB_LG_fx_Madds()

EMPLOYMENT <- read_tsv(EMPLOYMENT_PATH) |>
  filter(!is.na(sector))

# Vectors that depend of Final Demand like number of employees.
TSIN_ALL <- EMPLOYMENT |>
  get_employment_matrices(SINALOA)

# Input template
TEMPLATE <- read_tsv(TEMPLATE_PATH)
SHOCKS_COL_NAME <- names(TEMPLATE)[[9]]

BIREGIONAL <- get_biregional(SINALOA)

# The 35 sectors
SECTORS <- unique(TEMPLATE$sector)

# Shiny Flags
MODE_ORIDEST <- "mode_oridest" # Flag for Origen y Destino Mode
MODE_SHOCKS <- "mode_shocks"
MODE_SLIDER <- "slider" # Flag for Slider Activation

SECTORS_STRUCTURE <- read_rds(ORI_DEST_PATH) |>
  get_sector_structure() |>
  `colnames<-`(SECTORS)

cs <- colSums(SECTORS_STRUCTURE)
stopifnot(
  "Structure calculation has failed." =
    all(near(cs, 1) | near(cs, 0))
)


# UI helpers ----

select_mode <- list(
  radioButtons("mode",
    "Modo de Operación:",
    choiceNames = c(
      "Modo 1: Inversión (Origen/Destino)",
      "Modo 2: Choques Manuales"
    ),
    choiceValues = c(MODE_ORIDEST, MODE_SHOCKS)
  )
)

numeric_or_slider <- radioButtons("numericORslider",
  "Tipo de Entrada:",
  choiceNames = c(
    "Barra Deslizante (Slider)",
    "Numérico (Teclado)"
  ),
  choiceValues = c(MODE_SLIDER, "numeric"),
  inline = TRUE
)

# Inputs for Mode 2 (Manual)
input_sectors_sinaloa <- SECTORS |>
  map(\(sector)
  numericInput(glue("{sector}_sinaloa"),
    glue("SINALOA: {sector}"),
    value = 0, min = 0
  ))

input_sectors_mexico <- SECTORS |>
  map(\(sector)
  numericInput(glue("{sector}_mexico"),
    glue("RESTO PAÍS: {sector}"),
    value = 0, min = 0
  ))

# The hidden logic panel (Wizard)
mode_params <- tabsetPanel(
  id = "mode_params",
  type = "hidden",
  # Panel 1: Mode Origen Destino
  tabPanel(
    "mode_oridest",
    br(),
    selectInput("oridest_sector", "Selecciona el sector a invertir:", choices = SECTORS, width = "100%"),
    numericInput("oridest_invest",
      "Monto a invertir (USD):",
      value = 0, min = 0, width = "100%"
    ),
    p(class = "text-muted", "Nota: Puede usar notación científica, ej: 100e3 = 100,000"),
    hr(),
    numeric_or_slider
  ),
  # Panel 2: Mode Manual Shocks
  tabPanel(
    "mode_shocks",
    h5("Ingreso Manual de Demandas (Millones MXN)"),
    fluidRow(
      # Scrollable areas for the long list of sectors
      column(6, tags$div(style = "height: 500px; overflow-y: auto; padding-right: 10px;", input_sectors_sinaloa)),
      column(6, tags$div(style = "height: 500px; overflow-y: auto; padding-right: 10px;", input_sectors_mexico))
    )
  )
)

# UI ----

ui <- bs4DashPage(
  title = "Calculadora Insumo Producto",
  dark = NULL,

  # 1. Header
  header = bs4DashNavbar(
    title = dashboardBrand(
      title = "MIP Sinaloa",
      color = "primary",
      href = "https://github.com/elbecerrasoto/sincal", # URL ADDED HERE
      image = "https://adminlte.io/themes/v3/dist/img/AdminLTELogo.png"
    )
  ),

  # 2. Sidebar
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    bs4SidebarMenu(
      id = "sidebar_menu",
      bs4SidebarHeader("Configuración General"),
      div(
        class = "px-3",
        textInput("experiment_name", "Nombre del Experimento", placeholder = "Ej. Nueva Planta"),
        numericInput("tipo_cambio", "Tipo de Cambio (MXN/USD)", value = MXN_USD, min = 0),
        hr(),
        select_mode[[1]], # Mode Selector
      )
    )
  ),

  # 3. Body
  body = bs4DashBody(
    shinyFeedback::useShinyFeedback(),

    # Description / Helper Text (Collapsible)
    bs4Card(
      title = "Instrucciones",
      status = "gray",
      solidHeader = FALSE,
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      p("Incluye dos modos. El Modo 1 desglosa la inversión a través de las Matrices de Origen y Destino.
        Este modo necesita el monto a invertir en USD y el sector.
        El Modo 2 da total control manual (Millones de MXN).
        También se puede subir un archivo '.tsv' o '.xlsx' con una columna 'shocks_millones_mxn'.")
    ),

    # Row 1: KPI / Totals (Value Boxes)
    uiOutput("totals"),

    # Row 2: Main Interaction Area
    fluidRow(
      # Left Column: Logic / Inputs
      column(
        width = 6,
        bs4Card(
          title = "Parámetros de Entrada",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          mode_params # The hidden tabset
        )
      ),

      # Right Column: Regional Splits & Files
      column(
        width = 6,
        # Only show Splits card if in Mode 1
        conditionalPanel(
          condition = glue("input.mode == '{MODE_ORIDEST}'"),
          bs4Card(
            title = "Distribución Regional (Sinaloa vs Resto)",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            tags$div(
              style = "max-height: 450px; overflow-y: auto;",
              uiOutput("splits")
            )
          )
        ),

        # File Operations Card
        bs4Card(
          title = "Gestión de Archivos",
          status = "secondary",
          solidHeader = TRUE,
          width = 12,
          fileInput("uploaded", "Cargar archivo .tsv o .xlsx (Shocks MXN)", accept = c(".tsv", ".xlsx")),
          downloadButton("download", "Descargar Resultados", class = "btn-success btn-block")
        )
      )
    ),

    # Row 3: Results Table
    fluidRow(
      column(
        width = 12,
        bs4Card(
          title = "Tabla Detallada de Resultados",
          status = "primary",
          width = 12,
          collapsible = TRUE,
          DT::dataTableOutput("output_tab")
        )
      )
    )
  )
)

# server ----

server <- function(input, output, session) {
  observeEvent(input$mode, {
    updateTabsetPanel(inputId = "mode_params", selected = input$mode)
  })

  selected_structure <- reactive({
    req(input$oridest_sector)
    SECTORS_STRUCTURE[, input$oridest_sector, drop = TRUE] |>
      set_names(SECTORS)
  })


  output$splits <- renderUI({
    req(input$mode == MODE_ORIDEST)

    # Use HTML formatting for the label to make it readable in the dashboard
    slider_text <- "<b>{str_to_upper(sector)}</b><br>
    <small>Demanda: {format(round(input$oridest_invest * sector_struct, ROUND), big.mark=',')} USD
    ({round(sector_struct*100, ROUND)}%)</small><br>
    <i>% que se queda en Sinaloa:</i>"

    non_zero_sorted <- selected_structure() |>
      discard(~ near(.x, 0)) |>
      sort(decreasing = TRUE)

    if (length(non_zero_sorted) == 0) {
      return(div(class = "text-center", p("Este sector no genera demanda directa de insumos.")))
    }

    if (input$numericORslider == MODE_SLIDER) {
      out <- non_zero_sorted |>
        imap(\(sector_struct, sector)
        sliderInput(sector, HTML(glue(slider_text)), value = 0.5, min = 0, max = 1))
    } else {
      out <- non_zero_sorted |>
        imap(\(sector_struct, sector)
        numericInput(sector, HTML(glue(slider_text)), value = 0.5, min = 0, max = 1))
    }
    out
  })


  captured_splits <- reactive({
    req(input$mode == MODE_ORIDEST)

    splits_sin <- rep(0, length(SECTORS)) |> set_names(SECTORS)

    for (sector in SECTORS) {
      if (!is.null(input[[sector]])) {
        splits_sin[SECTORS %in% sector] <- input[[sector]]
      }
    }

    splits_nat <- 1 - splits_sin

    c(splits_sin, splits_nat)
  })

  shocks_millones_mxn <- reactive({
    if (input$mode == MODE_ORIDEST) {
      out <- captured_splits() * selected_structure() * input$tipo_cambio * input$oridest_invest / MIP_SCALE
    } else {
      sectors70 <- c(str_c(SECTORS, "_sinaloa"), str_c(SECTORS, "_mexico"))
      out <- map_dbl(sectors70, ~ input[[.x]])
    }
    out
  })


  template <- reactive({
    shocks_millones_mxn_V <- shocks_millones_mxn()

    if (input$mode == MODE_ORIDEST) {
      out <- TEMPLATE |>
        mutate(
          experiment_name = input$experiment_name,
          date = Sys.time(),
          use_origen_destino = TRUE,
          origen_destino_sector = input$oridest_sector,
          origen_destino_structure = rep(selected_structure(), 2),
          split = captured_splits(),
          investment_usd = input$oridest_invest,
          exrate = input$tipo_cambio,
          shocks_millones_mxn = shocks_millones_mxn_V
        )
    } else {
      out <- TEMPLATE |>
        mutate(
          experiment_name = input$experiment_name,
          date = Sys.time(),
          use_origen_destino = FALSE,
          origen_destino_sector = NA,
          origen_destino_structure = NA,
          split = NA,
          investment_usd = sum(shocks_millones_mxn_V) * MIP_SCALE / input$tipo_cambio,
          exrate = input$tipo_cambio,
          shocks_millones_mxn = shocks_millones_mxn_V
        )
    }
    out
  })


  # Main Calculation
  pib <- reactive({
    SINALOA$L %*% shocks_millones_mxn() |> as.double()
  })

  # Main Calculation
  empleos <- reactive({
    map(TSIN_ALL, \(M) M %*% shocks_millones_mxn() |> as.double()) |>
      as_tibble()
  })

  # Present the results
  results <- reactive({
    template_V <- template()
    results_intermediate <- empleos() |>
      mutate(pib = pib()) |>
      relocate(pib)

    breakdown_results_into_effects(BIREGIONAL, results_intermediate, template_V) |>
      left_join(template_V, join_by(scian, region, ))
  })


  output$totals <- renderUI({
    pib_val <- pib()
    empleos_val <- empleos()$empleos

    N <- length(pib_val)

    # Helper for bs4Dash value boxes
    ui_value_box <- function(value, legend, icon_name, color) {
      bs4ValueBox(
        value = value |> sum() |> round(ROUND) |> format(big.mark = ","),
        subtitle = legend,
        icon = icon(icon_name),
        color = color,
        width = 12
      )
    }

    # Layout using fluidRow/columns for the 4 boxes
    fluidRow(
      column(3, ui_value_box(pib_val, "Impacto PIB (Millones MXN)", "money-bill-wave", "success")),
      column(3, ui_value_box(empleos_val, "Impacto Empleos", "users", "primary")),
      column(3, ui_value_box(pib_val[1:(N / 2)], "Sinaloa: PIB", "map-marker-alt", "info")),
      column(3, ui_value_box(empleos_val[1:(N / 2)], "Sinaloa: Empleos", "user-tie", "info"))
    )
  })


  output$download <- downloadHandler(
    filename = function() {
      glue("{input$experiment_name}_{Sys.Date()}.tsv")
    },
    content = function(file) write_tsv(results(), file)
  )

  output$output_tab <- DT::renderDataTable(
    {
      AVOID_CLUTTER <- c(
        "experiment_name", "date", "use_origen_destino",
        "origen_destino_sector", "origen_destino_structure", "split",
        "investment_usd", "exrate"
      )

      results() |>
        select(-any_of(AVOID_CLUTTER))
    },
    options = list(scrollX = TRUE)
  )


  non_validated <- reactive({
    req(input$uploaded)
    path <- input$uploaded$datapath

    read_app_input <- function(path) {
      ext <- tools::file_ext(path)
      switch(ext,
        tsv = read_tsv(path),
        xlsx = openxlsx::read.xlsx(path) |> as_tibble()
      )
    }

    tryCatch(read_app_input(path), error = \(e) glue("{e}"))
  })


  observeEvent(non_validated(), {
    uploaded <- non_validated()

    if (!is_tibble(uploaded)) shinyFeedback::feedbackDanger("uploaded", TRUE, "Error en la tabla")
    req(is_tibble(uploaded))

    n_sectors <- length(TEMPLATE$sector)

    uploaded_shocks <- uploaded[[SHOCKS_COL_NAME]]

    correct_name <- SHOCKS_COL_NAME %in% names(uploaded)
    correct_size <- nrow(uploaded) == length(TEMPLATE$sector)
    correct_type <- is.numeric(uploaded_shocks) && all(uploaded_shocks >= 0)

    shinyFeedback::hideFeedback("uploaded")

    err_name <- glue("No existe {SHOCKS_COL_NAME}")
    err_size <- glue("{SHOCKS_COL_NAME} debe medir {n_sectors}")
    err_type <- glue("{SHOCKS_COL_NAME} NO es >= 0")

    # rstyler: off
    if      (!correct_name) shinyFeedback::feedbackDanger("uploaded", TRUE, err_name)
    else if (!correct_size) shinyFeedback::feedbackDanger("uploaded", TRUE, err_size)
    else if (!correct_type) shinyFeedback::feedbackDanger("uploaded", TRUE, err_type)
    else shinyFeedback::feedbackSuccess("uploaded", TRUE, "Carga Completada")
    # rstyler: on
  })



  uploaded_shocks <- reactive({
    msg <- "Error, por favor revisa tus datos de entrada"
    uploaded <- non_validated()
    validate(need(is_tibble(uploaded), msg))

    req(is_tibble(uploaded))

    n_sectors <- length(TEMPLATE$sector)

    uploaded_shocks <- uploaded[[SHOCKS_COL_NAME]]

    correct_name <- SHOCKS_COL_NAME %in% names(uploaded)
    correct_size <- nrow(uploaded) == length(TEMPLATE$sector)
    correct_type <- is.numeric(uploaded_shocks) && all(uploaded_shocks >= 0)

    validate(
      need(correct_name && correct_size && correct_type, msg)
    )

    uploaded_shocks
  })


  observeEvent(uploaded_shocks(), {
    updateRadioButtons(
      session = session,
      inputId = "mode",
      selected = MODE_SHOCKS
    )
  })


  update_manual_shocks <- function(new_shocks) {
    shocks_ids <- c(str_c(SECTORS, "_sinaloa"), str_c(SECTORS, "_mexico"))

    for (idx in seq_along(shocks_ids)) {
      input_id <- shocks_ids[[idx]]
      new_value <- new_shocks[[idx]]

      updateNumericInput(
        session = session,
        inputId = input_id,
        value = new_value
      )
    }
  }

  observeEvent(captured_splits(), {
    req(input$mode == MODE_ORIDEST)
    shocks_millones_mxn() |>
      update_manual_shocks()
  })


  observeEvent(uploaded_shocks(), {
    req(input$mode == MODE_ORIDEST)
    uploaded_shocks() |>
      update_manual_shocks()
  })
}

shinyApp(ui = ui, server = server)
