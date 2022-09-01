equipotential <- function(x,
                          var,
                          nclass = 8,
                          breaks,
                          mask,
                          buffer, 
                          xcoords,
                          ycoords) {
  
  if (!missing(buffer)){
    mask_b <- sf::st_buffer(mask, buffer)
    inter <- st_intersects(x = x, y = mask_b)
    inout <- sapply(inter, function(x)if(length(x)>0){1}else{NA})
    x[[var]] <- inout * x[[var]]
  }
  
  iso <- mapiso(x = x, var = var, 
                breaks = breaks, 
                nbreaks = nclass, 
                mask = mask)
  names(iso)[1:3] <- c("id", "min", "max")
  iso$center <- iso$min + (iso$max - iso$min) / 2
  
  return(iso)
}