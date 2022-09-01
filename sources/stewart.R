stewart <- function(knownpts,unknownpts, matdist, varname, 
                    typefct = "exponential", span, beta, resolution, mask, 
                    bypassctrl = FALSE, longlat = TRUE,  returnclass = "sp"){
  .Deprecated(new = "potential", package = "potential", 
              msg = paste0("stewart(), rasterStewart(), plotStewart(), ",
                           "quickStewart(), isopoly(), mcStewart() and smoothy() ", 
                           "are deprecated. Use 'potential' package for all ", 
                           "potential-related functions."))
  res <- prepdata(knownpts = knownpts, unknownpts = unknownpts, 
                  matdist = matdist, bypassctrl = bypassctrl, longlat = longlat,
                  mask = mask, resolution = resolution) 
  matdens <- ComputeInteractDensity(matdist = res$matdist, typefct = typefct,
                                    beta = beta, span = span)
  matopport <- ComputeOpportunity(knownpts = res$knownpts, matdens = matdens, 
                                  varname = varname)
  unknownpts <- ComputePotentials(unknownpts = res$unknownpts, 
                                  matopport = matopport)
  if(returnclass=="sp"){unknownpts <- suppressWarnings(as(unknownpts, "Spatial"))}
  return(unknownpts)
}