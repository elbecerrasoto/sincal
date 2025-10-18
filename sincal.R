library(tidyverse)
library(glue)
source("helper.R")

# ---- globals

MIP <- "data/mip_sinaloa.tsv"
EMPLOYMENT <- "data/empleo_sinaloa.tsv"
ORI_DEST <- "data/origen_destino.rds"

OUTPUT_DIR <- "results"
OUTPUT_TAB <- glue("{OUTPUT_DIR}/results.tsv")


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

# ---- read data

sinaloa <- read_tsv(MIP) |>
  get_ZAB_LG_fx_Madds()

employment <- read_tsv(EMPLOYMENT) |>
  filter(!is.na(sector))

SECTORS <- unique(employment$sector)

origen_destino_all <- read_rds(ORI_DEST)

# ---- employment

# Calculate employment matrices
etype <- c("empleos", "formales", "informales")

Tsin <- employment[etype] |>
  map(get_T, L = sinaloa$L, x = sinaloa$x)

Tsin <- set_names(Tsin, etype)

# ---- sector structure

sector_structure <- get_sector_structure(origen_destino_all)
colnames(sector_structure) <- SECTORS

# ---- multipliers

# results$directos <- rep(1, N_SECTORS) # direct
# results$indirectos <- colSums(sinaloa$M1a) # indirect
# results$desbordamiento <- colSums(sinaloa$M2a) # spillover
# results$retroalimentacion <- colSums(sinaloa$M3a) # feedback

# ---- write output

# write_tsv(results, OUTPUT)
