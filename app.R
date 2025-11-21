library(shiny)
library(tidyverse)
library(glue)
# library(DT)
# library(bs4Dash)
source("app_helper.R")

# globals ----

MIP_PATH <- "data/mip_sinaloa.tsv"
EMPLOYMENT_PATH <- "data/empleos_impuestos.tsv"
TEMPLATE_PATH <- "data/input_base.tsv"
ORI_DEST_PATH <- "data/origen_destino.rds"

MXN_USD <- 18.5 # Exchange rate MXN to USD
ROUND <- 2 # Rounding of numbers for printing
MIP_SCALE <- 1e6 # The scale input of the MIP, in this case millions of MXN

# Leontieff Stuff
SINALOA <- read_tsv(MIP_PATH) |>
  get_ZAB_LG_fx_Madds()

EMPLOYMENT <- read_tsv(EMPLOYMENT_PATH) |>
  filter(!is.na(sector))

# Vectors that depend of Final Demand like number of employees.
TSIN_ALL <- EMPLOYMENT |>
  get_employment_matrices(SINALOA)

# Input template
TEMPLATE <- read_tsv(TEMPLATE_PATH)

BIREGIONAL <- get_biregional(SINALOA)

# The 35 sectors
SECTORS <- unique(TEMPLATE$sector)

# Shiny Flags
MODE_ORIDEST <- "mode_oridest" # Flag for Origen y Destino Mode
MODE_SLIDER <- "slider" # Flag for Slider Activation
MODE_EFFECTS <- "mode_effects" # Flag for adding the effects to the result table

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
  radioButtons("template_mode",
    "Seleciona el modo a utilizar:",
    choiceNames = c(
      "Modo 1: Generar inversiones a partir de las matrices de Origen y Destino.",
      "Modo 2: Generar inversiones a partir de ingresar manualmente los choques en la demanda final."
    ),
    choiceValues = c(MODE_ORIDEST, "mode_shocks")
  ),
  radioButtons("effect_breakdown",
    "Deseas desglozar los resultados en efectos directos, indirectos, de desbordamiento y de retroalimentaión:",
    choiceNames = c(
      "Sí. (Genera tablas con muchas columnas)",
      "No. (Los efectos pueden ser calculados manualment a partir de los porcentajes dados)"
    ),
    choiceValues = c(MODE_EFFECTS, glue("{MODE_EFFECTS}_NO")),
    selected = glue("{MODE_EFFECTS}_NO")
  )
)

numeric_or_slider <- radioButtons("numericORslider",
  "¿Cómo deseas ingresear los valores?",
  choiceNames = c(
    "A través de una barra deslizante\n(primera vez, fácil de usar, prototipado).",
    "A través de teclear los valores\n(exactitud, corrida final)."
  ),
  choiceValues = c(MODE_SLIDER, "numeric")
)

input_sectors_sinaloa <- SECTORS |>
  map(\(sector)
  numericInput(glue("{sector}_sinaloa"),
    glue(
      "SINALOA: {str_to_upper(sector)}
                     Cantidad demandada dentro de Sinaloa en millones de pesos
                     del sector {sector}"
    ),
    value = 0, min = 0
  ))


input_sectors_mexico <- SECTORS |>
  map(\(sector)
  numericInput(glue("{sector}_mexico"),
    glue(
      "MEXICO: {str_to_upper(sector)}
                     Cantidad demandada fuera de Sinaloa en millones de pesos
                     del sector {sector}"
    ),
    value = 0, min = 0
  ))

mode_params <- tabsetPanel(
  id = "mode_params",
  type = "hidden",
  tabPanel(
    "mode_oridest",
    numeric_or_slider,
    selectInput("oridest_sector", "Selecciona el sector:", choices = SECTORS),
    numericInput("oridest_invest", "Ingresa el monto a invertir en USD: (Si deseas ingresarlo en miles de dolares puedes usa e3 al final de la cantidad, por ejemplo 100e3 serían 100,000 USD)", value = 0, min = 0),
  ),
  tabPanel(
    "mode_shocks",
    fluidRow(
      column(6, input_sectors_sinaloa),
      column(6, input_sectors_mexico)
    )
  ),
)

# UI ----

ui <- fluidPage(
  h1("Calculadora Insumo Producto Sinaloa"),
  p("
    Incluye dos modos, el primer modo desgloza la inversión a traves de
    las Matrices de Origen y Destino (https://www.inegi.org.mx/programas/tod/2018/#tabulados).
    Este modo necesita el monto a invertir en USD y seleccionar en que sector se realizará la inversión.
    Se puede hacer un desgloce más fino al especificar cuanto del capital se quedará dentro de Sinaloa y cuanto fuera.
    El segundo modo da total control al permitir desglozar la inversión de cualquier manera.
    IMPORTANTE: en este modo las cantidades a ingresar están en millones de pesos.
    "),
  sidebarLayout(
    sidebarPanel(
      select_mode,
      textInput("experiment_name", "Nombre del Experimento"),
      numericInput("tipo_cambio", "Tipo de Cambio MXN a USD", value = MXN_USD, min = 0)
    ),
    mainPanel(uiOutput("totals"), downloadButton("download", "Descargar Resultados")),
  ),
  mode_params,
  uiOutput("splits"),
  h1("Tabla de Resultados"),
  DT::dataTableOutput("output_tab"),
)

# server ----

server <- function(input, output) {
  observeEvent(input$template_mode, {
    updateTabsetPanel(inputId = "mode_params", selected = input$template_mode)
  })

  selected_structure <- reactive({
    SECTORS_STRUCTURE[, input$oridest_sector, drop = TRUE] |>
      set_names(SECTORS)
  })


  output$splits <- renderUI({
    req(input$template_mode == MODE_ORIDEST)

    slider_text <- "{str_to_upper(input$oridest_sector)}: {str_to_upper(sector)}
                           El sector: <i>{input$oridest_sector}</i> demanda
                           {round(input$oridest_invest * sector_struct,ROUND)} USD
                            ({round(sector_struct*100,ROUND)}% de la inversión)
                           del sector <i>{sector}</i>.
                           ¿Qué porcentaje de esta inversión debería quedarse en Sinaloa?
                           (el resto se iría a otros estados de la República)."

    non_zero_sorted <- selected_structure() |>
      discard(~ near(.x, 0)) |>
      sort(, decreasing = TRUE)

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
    req(input$template_mode == MODE_ORIDEST)

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
    if (input$template_mode == MODE_ORIDEST) {
      out <- captured_splits() * selected_structure() * input$tipo_cambio * input$oridest_invest / MIP_SCALE
    } else {
      sectors70 <- c(str_c(SECTORS, "_sinaloa"), str_c(SECTORS, "_mexico"))
      out <- map_dbl(sectors70, ~ input[[.x]])
    }
    out
  })


  template <- reactive({
    Vshocks_millones_mxn <- shocks_millones_mxn()

    if (input$template_mode == MODE_ORIDEST) {
      out <- TEMPLATE |>
        mutate(
          experiment_name = input$experiment_name,
          date = Sys.time(),
          use_origen_destino = input$template_mode == MODE_ORIDEST,
          origen_destino_sector = input$oridest_sector,
          origen_destino_structure = rep(selected_structure(), 2),
          split = captured_splits(),
          investment_usd = input$oridest_invest,
          exrate = input$tipo_cambio,
          shocks_millones_mxn = Vshocks_millones_mxn
        )
    } else {
      out <- TEMPLATE |>
        mutate(
          experiment_name = input$experiment_name,
          date = Sys.time(),
          use_origen_destino = input$template_mode == MODE_ORIDEST,
          origen_destino_sector = NA,
          origen_destino_structure = NA,
          split = NA,
          investment_usd = sum(Vshocks_millones_mxn) * MIP_SCALE / input$tipo_cambio,
          exrate = input$tipo_cambio,
          shocks_millones_mxn = Vshocks_millones_mxn
        )
    }
    out
  })

  main_shocks <- reactive({
    template()$shocks_millones_mxn
  })

  # Main Calculation
  pib <- reactive({
    SINALOA$L %*% main_shocks() |> as.double()
  })

  # Main Calculation
  empleos <- reactive({
    map(TSIN_ALL, \(M) M %*% main_shocks() |> as.double()) |>
      as_tibble()
  })

  # Present the results
  raw_results <- reactive({
    results_intermediate <- empleos() |>
      mutate(pib = pib()) |>
      relocate(pib)

    if (input$effect_breakdown == MODE_EFFECTS) {
      breffects <- breakdown_results_into_effects(BIREGIONAL, results_intermediate)
      out <- bind_cols(results_intermediate, BIREGIONAL, breffects)
    } else {
      out <- bind_cols(results_intermediate, BIREGIONAL)
    }
    out
  })

  results <- reactive({
    bind_cols(template(), raw_results())
  })

  # output$input_tab <- renderDataTable(
  #   template()
  # )

  output$totals <- renderUI({
    pib <- pib()
    empleos <- empleos()$empleos

    N <- length(pib)

    ui_value_box <- function(value, legend) {
      bs4Dash::valueBox(
        value = value |> sum() |> round(ROUND),
        subtitle = legend
      )
    }

    list(
      fluidRow(
        column(6, ui_value_box(pib, "Impacto en el PIB en millones de pesos")),
        column(6, ui_value_box(empleos, "Impacto en el número de empleos"))
      ),
      fluidRow(
        column(6, ui_value_box(pib[1:(N / 2)], "Sinaloa:\nImpacto en el PIB en millones de pesos")),
        column(6, ui_value_box(pib[(N / 2 + 1):N], "Resto del País:\nImpacto en el PIB en millones de pesos"))
      ),
      fluidRow(
        column(6, ui_value_box(empleos[1:(N / 2)], "Sinaloa:\nImpacto en el número de empleos")),
        column(6, ui_value_box(empleos[(N / 2 + 1):N], "Resto del País:\nImpacto en el número de empleos"))
      )
    )
  })


  output$download <- downloadHandler(
    filename = function() {
      glue("{input$experiment_name}_{Sys.Date()}.tsv")
    },
    content = function(file) write_tsv(results(), file)
  )

  output$output_tab <- DT::renderDataTable({
    results()
  })
}

shinyApp(ui = ui, server = server)
