potential <- function(x, y, d, var, fun, span, beta) {
  result <- prepare_data(x = x, y = y, d = d)
  matdens <- interact_density(
    d = result$d,
    fun = fun, beta = beta, span = span
  )
  
  pot <- apply(
    X = result$x[, var, drop = FALSE], MARGIN = 2,
    FUN = compute_potentials, matdens
  )
  
  if (length(var) == 1) {
    pot <- as.numeric(pot)
  }
  
  return(pot)
}


# Internal functions
#' @importFrom sf st_drop_geometry
prepare_data <- function(x, y, d) {
  d <- use_matrix(d = d, x = x, y = y)
  x <- st_drop_geometry(x)
  y <- st_drop_geometry(y)
  return(list(x = x, y = y, d = d))
}


use_matrix <- function(d, x, y) {
  i <- factor(row.names(x), levels = row.names(x))
  j <- factor(row.names(y), levels = row.names(y))
  d <- d[levels(i), levels(j)]
  return(round(d, digits = 8))
}


interact_density <- function(d, fun, beta, span) {
  if (fun == "p") {
    alpha <- (2^(1 / beta) - 1) / span
    matDens <- (1 + alpha * d)^(-beta)
  } else if (fun == "e") {
    alpha <- log(2) / span^beta
    matDens <- exp(-alpha * d^beta)
  }
  return(matDens)
}

compute_potentials <- function(x, matdens, var) {
  pot <- apply(x * matdens, 2, sum, na.rm = TRUE)
}