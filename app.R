library(shiny)
library(tidyverse)

# globals ----

TEMPLATE <- read_tsv("data/input_base.tsv")

MXN_USD <- 18.5
DATE <- Sys.Date() # later make it reactive upon everything
SECTORS <- unique(TEMPLATE$sector)

MODE_ORIDEST <- "mode_oridest"

# UI helpers ----

select_mode <- radioButtons("template_mode",
  "Seleciona la plantilla de entrada a generar:",
  choiceNames = c(
    "A partir de las matrices de Origen y Destino.",
    "A partir de ingresar manualmente los choques en la demanda final."
  ),
  choiceValues = c(MODE_ORIDEST, "mode_shocks")
)


mode_params <- tabsetPanel(
  id = "mode_params",
  type = "hidden",
  tabPanel(
    "mode_oridest",
    selectInput("oridest_sector", "Selecciona el sector:", SECTORS),
    numericInput("oridest_invest", "Ingresa el monto a invertir en USD:", 0, min = 0)
  ),
  tabPanel(
    "mode_shocks"
  ),
)

# UI ----

ui <- fluidPage(
  select_mode,
  textInput("experiment_name", "Nombre del Experimento"),
  numericInput("tipo_cambio", "Tipo de Cambio MXN a USD", MXN_USD, min = 0),
  mode_params,
  dataTableOutput("template_tab")
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
          investment_usd = input$oridest_invest,
          exrate = input$tipo_cambio
        )
    } else {
      out <- TEMPLATE |>
        mutate(
          experiment_name = input$experiment_name,
          date = Sys.time(),
          use_origen_destino = input$template_mode == MODE_ORIDEST,
          origen_destino_sector = NA,
          investment_usd = NA,
          exrate = input$tipo_cambio
        )
    }

    out
  })


  output$template_tab <- renderDataTable(template())
}

shinyApp(ui = ui, server = server)
