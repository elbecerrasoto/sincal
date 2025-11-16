library(shiny)
library(tidyverse)
library(glue)

# globals ----

TEMPLATE <- read_tsv("data/input_base.tsv")

MXN_USD <- 18.5
SECTORS <- unique(TEMPLATE$sector)

MODE_ORIDEST <- "mode_oridest"

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
    req(input$experiment_name != "")
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
      out$origen_destino_structure <- rep(SECTORS_STRUCTURE[, input$oridest_sector], 2)
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

  selected_structure <- reactive({
    SECTORS_STRUCTURE[, input$oridest_sector, drop = TRUE] |>
      set_names(SECTORS)
  })

  output$debug <- renderText(selected_structure())
  
  # output$splits <- (
  #   {
  #     map(non_zero_sectors(), \(sector)
  #         sliderInput(sector, glue("Porcentaje de {sector} que se queda"), value = 0.5, min = 0, max = 1))
  #   }
  # )

  output$template_tab <- renderDataTable(template())
}

shinyApp(ui = ui, server = server)
