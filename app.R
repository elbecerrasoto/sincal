library(shiny)
library(tidyverse)
library(glue)

# globals ----

TEMPLATE <- read_tsv("data/input_base.tsv")

MXN_USD <- 18.5
ROUND <- 2
SECTORS <- unique(TEMPLATE$sector)

MODE_ORIDEST <- "mode_oridest"
MODE_SLIDER <- "slider"

ORI_DEST <- "data/origen_destino.rds"

get_sector_structure <- function(origen_destino_all) {
  origen_destino <- reduce(origen_destino_all, `+`)
  origen_destino[origen_destino < 0] <- 0

  relative_buys <- function(col) {
    total <- sum(col)
    if (total > 0) {
      col / total
    } else {
      return(rep(0, length(col)))
    }
  }

  apply(origen_destino, 2, relative_buys)
}

SECTORS_STRUCTURE <- read_rds(ORI_DEST) |>
  get_sector_structure() |>
  `colnames<-`(SECTORS)

cs <- colSums(SECTORS_STRUCTURE)
stopifnot(
  "Structure calculation has failed." =
    all(near(cs, 1) | near(cs, 0))
)

# UI helpers ----

select_mode <- radioButtons("template_mode",
  "Seleciona la plantilla de entrada a generar:",
  choiceNames = c(
    "A partir de las matrices de Origen y Destino.",
    "A partir de ingresar manualmente los choques en la demanda final."
  ),
  choiceValues = c(MODE_ORIDEST, "mode_shocks")
)

select_input <- radioButtons("numericORslider",
  "¿Cómo deseas ingresear los valores?",
  choiceNames = c(
    "A través de una barra deslizante\n(primera vez, fácil de usar, prototipado).",
    "A través de teclear los valores\n(exactitud, corrida final)."
  ),
  choiceValues = c(MODE_SLIDER, "numeric")
)


mode_params <- tabsetPanel(
  id = "mode_params",
  type = "hidden",
  tabPanel(
    "mode_oridest",
    selectInput("oridest_sector", "Selecciona el sector:", choices = SECTORS),
    numericInput("oridest_invest", "Ingresa el monto a invertir en USD:", value = 0, min = 0),
  ),
  tabPanel(
    "mode_shocks"
  ),
)

# UI ----

ui <- fluidPage(
  select_mode,
  select_input,
  textInput("experiment_name", "Nombre del Experimento"),
  numericInput("tipo_cambio", "Tipo de Cambio MXN a USD", value = MXN_USD, min = 0),
  mode_params,
  uiOutput("splits"),
  dataTableOutput("template_tab"),
  verbatimTextOutput("debug")
)

# server ----

server <- function(input, output) {
  observeEvent(input$template_mode, {
    updateTabsetPanel(inputId = "mode_params", selected = input$template_mode)
  })

  template <- reactive({
    if (input$template_mode == MODE_ORIDEST) {
      out <- TEMPLATE |>
        mutate(
          experiment_name = input$experiment_name,
          date = Sys.time(),
          use_origen_destino = input$template_mode == MODE_ORIDEST,
          origen_destino_sector = input$oridest_sector,
          origen_destino_structure = rep(selected_structure(), 2),
          split = 0.5,
          investment_usd = input$oridest_invest,
          exrate = input$tipo_cambio,
          shocks_millones_mxn = NA
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
          investment_usd = NA,
          exrate = input$tipo_cambio,
          shocks_millones_mxn = NA
        )
    }

    out
  })

  selected_structure <- reactive({
    SECTORS_STRUCTURE[, input$oridest_sector, drop = TRUE] |>
      set_names(SECTORS)
  })


  output$splits <- renderUI({
    slider_text <- "El sector: <i>{input$oridest_sector}</i> demanda
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


  # captured_splits <- reactive({
  #   splits_sin <- rep(0, length(SECTORS))
  #   splits_sin <- set_names(splits_sin, SECTORS)
  #
  #   for (iname in names(input)) {
  #     if (is.numeric(input[[iname]]) && any(SECTORS %in% iname)) {
  #       splits_sin[SECTORS %in% iname] <- input[[iname]]
  #     }
  #   }
  #
  #   splits_nat <- 1 - splits_sin
  #
  #   c(splits_sin, splits_nat)
  # })
  #
  output$debug <- renderText(selected_structure())
  output$template_tab <- renderDataTable(template())
}

shinyApp(ui = ui, server = server)
