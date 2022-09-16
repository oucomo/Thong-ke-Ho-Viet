plotStewart <- function(x, add = FALSE, 
                        breaks = NULL, typec = "equal", 
                        nclass = 5, legend.rnd = 0, 
                        col =  colorRampPalette(c("#FEA3A3","#980000"))){
  if (!is.null(breaks)){
    bks <- unique(breaks[order(breaks)])
  } else if (typec == "equal"){
    bks <- seq(from = cellStats(x, min), 
               to = cellStats(x, max), length.out = nclass+1)
  } else if (typec == "quantile"){
    bks <- quantile (x, probs = seq(0,1, by = 1/nclass))
  } else {
    stop('Enter a proper discretisation type: "equal" or "quantile"')
  }
  bks <- unique(bks)
  col <- col(length(bks)-1)
  plot(x, breaks = bks, legend = FALSE, axes = FALSE,
       box = FALSE, col = col,  add = add)
  
  nbks <- round(bks,legend.rnd)
  leglab <- rep(NA, (length(nbks)-1))
  for(i in 1:(length(nbks)-1)){
    leglab[i] <- paste("[", nbks[i], " - ", nbks[i+1],"[" ,sep="")
  }
  leglab[i] <- paste( substr(leglab[i],1, nchar(leglab[i])-1), "]", sep="")
  
  graphics::legend(x='topright', legend = rev(leglab), 
                   xpd=T,inset=c(-0.2,0), 
                   fill = rev(col), cex = 0.7, 
                   plot = TRUE, bty = "n", 
                   title = "Potentials")
  
  return(invisible(bks))
}