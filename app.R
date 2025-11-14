library(shiny)
source("ui_helper.R")


controls_oridest <- list()
controls_shocks <- list()



mode_params <- tabsetPanel(
  id = "mode_params",
  type = "hidden",
  tabPanel(
    "mode_oridest",
    numericInput("mean1", "mean", value = 1),
    numericInput("mean2", "mean", value = 1)
  ),
  tabPanel(
    "mode_shocks",
    numericInput("mean3", "mean", value = 1)
  ),
)


select_mode <- radioButtons("template_mode",
  "Seleciona la plantilla de entrada a generar:",
  choiceNames = c(
    "A partir de las matrices de Origen y Destino.",
    "A partir de ingresar manualmente los choques en la demanda final."
  ),
  choiceValues = c("mode_oridest", "mode_shocks")
)


ui <- fluidPage(
  select_mode,
  mode_params
)

server <- function(input, output) {
  observeEvent(input$template_mode, {
    updateTabsetPanel(inputId = "mode_params", selected = input$template_mode)
  })
}

shinyApp(ui = ui, server = server)
