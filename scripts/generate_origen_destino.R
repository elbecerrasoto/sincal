#/usr/bin/env Rcript
library(tidyverse)
library(readxl)
library(janitor)
library(furrr)

# ---- globals

CORES <- future::availableCores()
if (.Platform$OS.type != "windows") {
  plan(multicore, workers = CORES)
} else {
  plan(multisession, workers = CORES)
}

DIR <- "scripts/tabulados_TODFBCF/"
NAMES <- "scripts/subsector_nacional.txt"
MAP <- "scripts/ori_bir.tsv"

OUT <- "origen_destino.rds"

INPUTS <- readLines(NAMES) %>%
  str_c(DIR, .)

# ---- helpers

read_origen_destino <- function(path) {
  SKIP_ABOVE <- 6
  SKIP_BELOW <- 5
  x <- read_xlsx(path, col_names = FALSE) |>
    slice_tail(n = -SKIP_ABOVE) |>
    slice_head(n = -SKIP_BELOW)
  
  sectors <- x[[1]] |>
    janitor::make_clean_names()
  
  x <- x[, -(1:2)] |>
    mutate(across(everything(), ~ as.double(.x)))
  
  x <- x |>
    set_names(sectors)
  
  x |> as.matrix()
}

collapse_matrix <- function(M, mapped) {
  reduce_cols <- function(M, idxs) {
    reduce(map(
      idxs,
      \(i) M[, i]
    ), `+`)
  }
  
  reduce_rows <- function(M, idxs) {
    reduce(map(
      idxs,
      \(i) M[i, ]
    ), `+`)
  }
  
  col_reduced <- map(mapped, \(idxs)
                     reduce_cols(M, idxs)) %>%
    do.call(cbind, .)
  
  row_reduced <- map(mapped, \(idxs)
                     reduce_rows(col_reduced, idxs)) %>%
    do.call(rbind, .)
  
  row_reduced
}

# ---- main

ALL <- future_map(INPUTS, read_origen_destino)

map_ori_bir <- read_tsv(MAP, col_types = "ffff")

MAPVEC <- set_names(map_ori_bir$scian_bir, map_ori_bir$scian_ori)
which_MAPVEC <- function(iname) {
  which(MAPVEC == MAPVEC[[iname]])
}

mapped <- map(names(MAPVEC), which_MAPVEC) |>
  unique()

mapped <- set_names(mapped, unique(map_ori_bir$scian_bir))

all_collapsed <- map(ALL, \(M) collapse_matrix(M, mapped))
write_rds(all_collapsed, OUT)






