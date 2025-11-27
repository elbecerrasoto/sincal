library(shiny)
library(tidyverse)
library(glue)
library(DT)
library(bs4Dash)
library(shinyFeedback)

# Load your helper functions
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
# Note: get_ZAB_LG_fx_Madds is likely in helper.R (sourced by app_helper.R)
SINALOA <- read_tsv(MIP_PATH) |>
  get_ZAB_LG_fx_Madds()

EMPLOYMENT <- read_tsv(EMPLOYMENT_PATH) |>
  filter(!is.na(sector))

is.matrix(SINALOA$x)

tibbles_sinaloa <- imap(SINALOA, \(x, i)
if (is.matrix(x)) {
  as_tibble(x)
} else {
  tibble(i = x)
})

EMPLOYMENT |> write_tsv("dependientes_demanda_sinaloa.tsv")

all_tibs <- c(tibbles_sinaloa, list("dependientes_f" = EMPLOYMENT))

names(all_tibs)
map_lgl(all_tibs, is_tibble) |> all()

library(openxlsx)
WB <- createWorkbook()

imap(
  all_tibs,
  function(data, name) {
    addWorksheet(WB, name)
    writeData(WB, sheet = name, x = data)
  }
)

saveWorkbook(WB, "sinaloa_leontieff.xlsx")
