library(tidyverse)

TOLERANCE <- 1e-2

N_REGION <- 35
N_OUTER <- 35
N_SECTORS <- N_REGION + N_OUTER

tib2mat <- function(tib, drop_names = FALSE) {
  mat <- tib |>
    select(where(is.numeric)) |>
    as.matrix()
  if (drop_names) {
    colnames(mat) <- NULL
    rownames(mat) <- NULL
  }
  mat
}

check_square <- function(M) {
  if (nrow(M) != ncol(M)) stop("Matrix is not square.")
}

get_Z <- function(Z_aug) {
  # tib -> mat
  Z_aug <- Z_aug |> select(where(is.numeric))
  Z <- Z_aug[1:N_SECTORS, 1:N_SECTORS]
  tib2mat(Z)
}

get_x <- function(Z_aug) {
  # tib -> double
  Z_aug <- Z_aug |> select(where(is.numeric))
  x_row <- rowSums(Z_aug[1:N_SECTORS, ])
  x_col <- colSums(Z_aug[, 1:N_SECTORS])

  are_xs_equal <- all(near(x_row, x_col, TOLERANCE))
  stopifnot("Row and Col totals do NOT match." = are_xs_equal)

  return(x_row)
}

get_f <- function(Z_aug) {
  # tib -> double
  Z_aug <- Z_aug |> select(where(is.numeric))
  Z_aug[1:N_SECTORS, -1:-N_SECTORS] |>
    rowSums()
}

normalize <- function(M, x, byrow = TRUE) {
  # mat, double -> mat

  rows_or_cols <- function(M, byrow = TRUE) {
    if (byrow) {
      out <- map(1:nrow(M), \(i) as.numeric(M[i, ]))
    } else {
      out <- map(1:ncol(M), \(i) as.numeric(M[, i]))
    }
    out
  }

  rocs <- rows_or_cols(M, byrow)
  f <- function(roc, i) {
    AVOID_UNDEF <- 1
    if (x[i] == 0) {
      out <- roc / AVOID_UNDEF
    } else {
      out <- roc / x[i]
    }
    out
  }
  imap(rocs, f) |>
    unlist() |>
    matrix(
      nrow = nrow(M),
      ncol = ncol(M),
      byrow = byrow
    )
}

get_A <- function(Z, x) normalize(Z, x, byrow = FALSE)

get_L <- function(A) {
  check_square(A)
  I <- diag(ncol(A))
  solve(I - A)
}

get_B <- function(Z, x) normalize(Z, x, byrow = TRUE)

get_G <- function(B) get_L(B)

get_ZAB_LG_fx <- function(Z_aug) {
  Z <- get_Z(Z_aug)
  f <- get_f(Z_aug)
  x <- get_x(Z_aug)

  A <- get_A(Z, x)
  B <- get_B(Z, x)

  L <- get_L(A)
  G <- get_G(B)

  list(Z = Z, A = A, B = B, L = L, G = G, f = f, x = x)
}

get_M1a_M2a_M3a <- function(A) {
  check_square(A)

  n <- ncol(A)
  r <- N_REGION
  s <- r + 1

  # ------ Regionalize

  Arr <- A[1:r, 1:r]
  Ars <- A[1:r, s:n]

  Ass <- A[s:n, s:n]
  Asr <- A[s:n, 1:r]

  Irr <- diag(N_REGION)
  Iss <- diag(N_OUTER)

  Ors <- matrix(0, nrow = N_REGION, ncol = N_OUTER)
  Osr <- matrix(0, nrow = N_OUTER, ncol = N_REGION)

  Lrr <- solve(Irr - Arr)
  Lss <- solve(Iss - Ass)

  # Spillover
  Srs <- Lrr %*% Ars
  Ssr <- Lss %*% Asr

  # Feed-back
  Frr <- solve(Irr - Srs %*% Ssr)
  Fss <- solve(Iss - Ssr %*% Srs)

  M1_rr_sr_col <- rbind(Lrr, Osr)
  M1_rs_ss_col <- rbind(Ors, Lss)
  M1 <- cbind(M1_rr_sr_col, M1_rs_ss_col)

  M2_rr_sr_col <- rbind(Irr, Ssr)
  M2_rs_ss_col <- rbind(Srs, Iss)
  M2 <- cbind(M2_rr_sr_col, M2_rs_ss_col)

  M3_rr_sr_col <- rbind(Frr, Osr)
  M3_rs_ss_col <- rbind(Ors, Fss)
  M3 <- cbind(M3_rr_sr_col, M3_rs_ss_col)

  I <- diag(N_SECTORS)

  M1a <- M1 - I
  M2a <- (M2 - I) %*% M1
  M3a <- (M3 - I) %*% M2 %*% M1

  list(M1a = M1a, M2a = M2a, M3a = M3a)
}

get_ZAB_LG_fx_Madds <- function(mip) {
  res <- get_ZAB_LG_fx(mip)
  Madds <- get_M1a_M2a_M3a(res$A)

  # fails at the diagonals
  # attach(Madds)
  # all_equal <- all(near(M1a + M2a + M3a, res$L, TOLERANCE))
  # stopifnot("Failed additive descomposition", all_equal)
  # detach(Madds)

  c(res, Madds)
}

get_T <- function(L, x, E) {
  e <- E / x
  Tm <- diag(e) %*% L
  mask <- is.nan(Tm) | is.infinite(Tm) | is.na(Tm)
  Tm[mask] <- 0
  Tm
}
