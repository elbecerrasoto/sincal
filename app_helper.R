library(glue)
source("leontief.R")

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

breakdown_results_into_effects <- function(percents, totals, sector_info) {
  add_cols <- function(x, efecto) {
    x |>
      mutate(
        efecto = efecto,
        scian = sector_info$scian,
        region = sector_info$region,
        sorting_col = row_number()
      )
  }

  multiply_by_effect <- function(percent_col, totals) {
    totals |>
      mutate(across(
        everything(),
        \(pib) pib * percent_col
      ))
  }

  efectos <- imap(
    percents,
    \(percent_col, percent_effect)
    multiply_by_effect(percent_col, totals) |>
      add_cols(percent_effect)
  ) |> bind_rows()

  results <- totals |>
    add_cols("totales") |>
    bind_rows(efectos)

  results$efecto <- factor(results$efecto,
    levels = c(
      "totales", "directos",
      "indirectos", "desbordamiento",
      "retroalimentacion"
    ), ordered = TRUE
  )

  results |>
    relocate(efecto, scian, region) |>
    arrange(sorting_col, efecto) |>
    select(-sorting_col)
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
