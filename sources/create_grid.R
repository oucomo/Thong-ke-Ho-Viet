create_grid <- function(x, res) {
  bb <- st_bbox(x)
  rounder <- bb %% res
  bb[1:2] <- bb[1:2] - rounder[1:2]
  bb[3:4] <- bb[3:4] + res - rounder[3:4]
  cx <- seq(from = bb[1], to = bb[3], by = res)
  cy <- seq(from = bb[2], to = bb[4], by = res)
  
  g <- expand.grid(cx, cy)
  g <- data.frame(
    ID = 1:nrow(g),
    COORDX = g[, 1],
    COORDY = g[, 2]
  )
  g <- st_as_sf(g,
                coords = c("COORDX", "COORDY"),
                crs = st_crs(x), remove = FALSE
  )
  
  return(g)
}