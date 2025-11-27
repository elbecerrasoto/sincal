library(glue)
source("leontieff.R")

get_employment_matrices <- function(employment, sinaloa) {
  etype <- names(employment)[4:length(employment)]
  employment[etype] |>
    map(get_T, L = sinaloa$L, x = sinaloa$x) |>
    set_names(etype)
}


get_biregional <- function(sinaloa) {
  biregional_multipliers <- tibble(
    directos = rep(1, length(sinaloa$x)),
    indirectos = colSums(sinaloa$M1a),
    desbordamiento = colSums(sinaloa$M2a),
    retroalimentacion = colSums(sinaloa$M3a)
  )

  row_totals <- rowSums(biregional_multipliers)

  biregional_percents <- biregional_multipliers |>
    mutate(across(
      everything(),
      \(x) x / row_totals
    ))

  biregional_percents
}


breakdown_results_into_effects <- function(effects_percents, leontief_results) {
  imap(
    effects_percents,
    function(brcol, brcol_name) {
      leontief_results |>
        mutate(across(
          everything(),
          ~ .x * brcol
        )) |>
        rename_with(~ paste0(brcol_name, "_", .x))
    }
  )
}

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
