library(shiny)
library(tidyverse)

# globals ----

TEMPLATE <- read_tsv("data/input_base.tsv")

MXN_USD <- 18.5
DATE <- Sys.Date() # later make it reactive upon everything
TEMPLATE$date <- DATE
SECTORS <- unique(TEMPLATE$sector)

# UI helpers ----

select_mode <- radioButtons("template_mode",
  "Seleciona la plantilla de entrada a generar:",
  choiceNames = c(
    "A partir de las matrices de Origen y Destino.",
    "A partir de ingresar manualmente los choques en la demanda final."
  ),
  choiceValues = c("mode_oridest", "mode_shocks")
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
  textInput("experimento", "Nombre del Experimento"),
  numericInput("tipo_cambio", "Tipo de Cambio MXN a USD", MXN_USD, min = 0),
  mode_params,
  dataTableOutput("template_tab")
)

# server ----

server <- function(input, output) {
  observeEvent(input$template_mode, {
    updateTabsetPanel(inputId = "mode_params", selected = input$template_mode)
  })
 
  output$template_tab <- renderDataTable(TEMPLATE)
  
}

shinyApp(ui = ui, server = server)
