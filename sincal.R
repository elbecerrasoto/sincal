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
EMPLOYMENT <- "data/empleos_impuestos.tsv"
ORI_DEST <- "data/origen_destino.rds"
SPLITS <- "data/splits.tsv"

EMPLOYMENT_TIBBLE <- read_tsv(EMPLOYMENT) |>
  filter(!is.na(sector))
SECTORS <- EMPLOYMENT_TIBBLE$sector |> unique()

stopifnot(
  "Unrecognized input sector" =
    INPUT_SECTOR %in% SECTORS
)

OUTPUT_DIR <- "results"
OUTPUT <- glue("{OUTPUT_DIR}/{INPUT_SECTOR}.tsv")

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
  etype <- c("empleos", "formales", "informales", "impuestos", "valor_bruto")
  employment[etype] |>
    map(get_T, L = sinaloa$L, x = sinaloa$x) |>
    set_names(etype)
}

# ---- key data

sinaloa <- read_tsv(MIP) |>
  get_ZAB_LG_fx_Madds()

Tsin_all <- EMPLOYMENT_TIBBLE |>
  get_employment_matrices()

sectors_structure <- read_rds(ORI_DEST) |>
  get_sector_structure() |>
  `colnames<-`(SECTORS)

cs <- colSums(sectors_structure)
stopifnot(
  "Structure calculation has failed." =
    all(near(cs, 1) | near(cs, 0))
)

splits <- read_tsv(SPLITS)

splits_sums <- splits |>
  select(sin, out) |>
  rowSums()

stopifnot(
  "Splits don't sum up to 1." =
    all(near(splits_sums, 1))
)

# ---- results

splits_vec <- c(splits$sin, splits$out)

input_sector_structure <-
  sectors_structure[, which(SECTORS %in% INPUT_SECTOR), drop = TRUE] |>
  rep(2)

shocks <- input_sector_structure * splits_vec * INVESTMENT_MILLIONS_MXN

stopifnot(
  "Error with investment shocks." =
    near(sum(shocks), INVESTMENT_MILLIONS_MXN)
)

pib <- sinaloa$L %*% shocks |> as.double()
empleos <- map(Tsin_all, \(M) M %*% shocks |> as.double()) |>
  as_tibble()

results <- empleos |>
  mutate(pib = pib)

# ---- biregional effects

did <- tibble(
  directos = rep(1, N_SECTORS),
  indirectos = colSums(sinaloa$M1a),
  desbordamiento = colSums(sinaloa$M2a),
  retroalimentacion = colSums(sinaloa$M3a)
)

row_totals <- rowSums(did)

did <- did |>
  mutate(across(
    everything(),
    \(x) x / row_totals
  ))

breffects <- imap(
  did,
  function(x, i) {
    results |>
      mutate(across(
        everything(),
        ~ .x * x
      )) |>
      rename_with(~ paste0(i, "_", .x))
  }
)

results_breffects <- EMPLOYMENT_TIBBLE |>
  select(sector, region, scian) |>
  bind_cols(results, breffects)

# ---- write output

dir.create(OUTPUT_DIR, recursive = TRUE)
write_tsv(results_breffects, OUTPUT)
