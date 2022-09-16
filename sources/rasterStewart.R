rasterStewart <- function(x, mask = NULL){
  if(is(x, "sf")){x <- suppressWarnings(as(x, "Spatial"))}
  gridded(x) <- TRUE
  r <- raster(x)
  rasterx <- rasterize(x[!is.na(x$OUTPUT),], r, field = 'OUTPUT')
  if(!is.null(mask)){
    if(is(mask, "sf")){mask <- suppressWarnings(as(mask, "Spatial"))}
    projError(x, mask)
    rasterx <- mask(rasterx, mask = mask)
  }
  return(rasterx)
}