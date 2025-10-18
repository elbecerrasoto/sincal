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

EMPLOYMENT_TIBBLE <- read_tsv(EMPLOYMENT) |>
  filter(!is.na(sector))
SECTORS <- EMPLOYMENT_TIBBLE$sector |> unique()

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
  "structure calculation failed" =
    all(near(cs, 1) | near(cs, 0))
)


# ---- manipulations

split_vec <- c(
  rep(1, 35),
  rep(0, 35)
)

input_sector_structure <-
  sectors_structure[, which(SECTORS %in% INPUT_SECTOR), drop = TRUE] |>
  rep(2)

shocks <- input_sector_structure * split_vec * INVESTMENT_MILLIONS_MXN

# into an assert
near(sum(shocks), INVESTMENT_MILLIONS_MXN)

#
# effects_pib <- shocks70 |>
#   select(starts_with(SHOCK_MARK)) |>
#   map(\(sh) as.double(sinaloa$L %*% sh)) |>
#   as_tibble()
#
# new_names <- str_c(names(effects_pib), "_pib")
# effects_pib <- set_names(effects_pib, new_names)
#
# Leffects_employment <- vector(mode = "list", length = 3)
# for (Ttype in names(Tsin)) {
#   Tm <- Tsin[[Ttype]]
#   i_effects <- shocks70 |>
#     select(starts_with(SHOCK_MARK)) |>
#     map(\(sh) as.double(Tm %*% sh))
#   new_names <- str_c(names(i_effects), "_", Ttype)
#
#   i_effects <- i_effects |>
#     set_names(new_names) |>
#     as_tibble()
#
#   Leffects_employment[[Ttype]] <- i_effects
# }
# effects_employment <- bind_cols(Leffects_employment)
#
# results <- shocks70 |>
#   select(!starts_with(SHOCK_MARK)) |>
#   bind_cols(effects_pib, effects_employment)



# ---- multipliers

# results$directos <- rep(1, N_SECTORS) # direct
# results$indirectos <- colSums(sinaloa$M1a) # indirect
# results$desbordamiento <- colSums(sinaloa$M2a) # spillover
# results$retroalimentacion <- colSums(sinaloa$M3a) # feedback

# ---- write output

# dir.create("path", recursive = TRUE)
# write_tsv(results, OUTPUT)
