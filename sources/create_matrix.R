create_matrix <- function(x, y, checksize = TRUE, longlat = FALSE) {
  if (checksize) {
    nk <- nrow(x)
    nu <- nrow(y)
    if (nk * nu > 100000000 | nu > 10000000 | nk > 10000000) {
      stop(paste0(
        "Computation aborted. The distance matrix would probably ",
        "be too large. Use checksize = FALSE to bypass this control."
      ),
      call. = FALSE
      )
    }
  }
  
  if (!st_is_longlat(x)) {
    if (longlat) {
      x <- st_transform(x, 4326)
      y <- st_transform(y, 4326)
    }
  }
  d <- st_distance(x, y)
  mat <- as.vector(d)
  dim(mat) <- dim(d)
  dimnames(mat) <- list(row.names(x), row.names(y))
  return(mat)
}