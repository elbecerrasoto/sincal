library(glue)
source("helper.R")

get_employment_matrices <- function(employment, sinaloa) {
  etype <- names(employment)[4:length(employment)]
  employment[etype] |>
    map(get_T, L = sinaloa$L, x = sinaloa$x) |>
    set_names(etype)
}


get_biregional <- function(sinaloa) {
  biregional_multipliers <- tibble(
    directos = rep(1, N_SECTORS),
    indirectos = colSums(SINALOA$M1a),
    desbordamiento = colSums(SINALOA$M2a),
    retroalimentacion = colSums(SINALOA$M3a)
  )

  row_totals <- rowSums(biregional_multipliers)

  biregional_percents <- biregional_multipliers |>
    mutate(across(
      everything(),
      \(x) x / row_totals
    ))

  # biregional_percents <- biregional_percents |> rename_with(~ glue("{.x}_porcentaje"))

  biregional_percents
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
