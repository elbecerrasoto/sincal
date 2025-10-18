library(tidyverse)
library(glue)
source("helper.R")

# ---- globals

INPUT_SECTOR <- "industria_quimica_plasticos"
INVESTMENT_USD <- 1681978e3

USD_MXN <- 18.50
MIP_SCALE <- 1e6

INVESTMENT_MILLIONS_MXN <- USD_MXN * INVESTMENT_USD / MIP_SCALE

MIP <- "data/mip_sinaloa.tsv"
EMPLOYMENT <- "data/empleo_sinaloa.tsv"
ORI_DEST <- "data/origen_destino.rds"

SECTORS <- read_tsv("data/empleo_sinaloa.tsv")$sector |> unique()

stopifnot(
  "Unrecognized input sector" =
    INPUT_SECTOR %in% SECTORS
)

OUTPUT_DIR <- "results"
OUTPUT_TAB <- glue("{OUTPUT_DIR}/{INPUT_SECTOR}.tsv")

# ---- helpers

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

get_employment_matrices <- function(employment) {
  etype <- c("empleos", "formales", "informales")
  employment[etype] |>
    map(get_T, L = sinaloa$L, x = sinaloa$x) |>
    set_names(etype)
}

# ---- read data

sinaloa <- read_tsv(MIP) |>
  get_ZAB_LG_fx_Madds()

Tsin_all <- read_tsv(EMPLOYMENT) |>
  filter(!is.na(sector)) |>
  get_employment_matrices()

sector_structure <- read_rds(ORI_DEST) |>
  get_sector_structure() |>
  set_names(SECTORS)

# ---- multipliers

# results$directos <- rep(1, N_SECTORS) # direct
# results$indirectos <- colSums(sinaloa$M1a) # indirect
# results$desbordamiento <- colSums(sinaloa$M2a) # spillover
# results$retroalimentacion <- colSums(sinaloa$M3a) # feedback

# ---- write output

# dir.create("path", recursive = TRUE)
# write_tsv(results, OUTPUT)
