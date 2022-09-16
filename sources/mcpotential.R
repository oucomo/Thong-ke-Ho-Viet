mcpotential <- function(x, y, var, fun,
                        span, beta,
                        limit = 3 * span,
                        ncl, size = 500) {
  # launch multiple cores
  if (missing(ncl)) {
    ncl <- parallel::detectCores(all.tests = FALSE, logical = FALSE) - 1
    if (is.na(ncl)) {
      ncl <- 1
    }
    if (ncl == 0) {
      ncl <- 1
    }
  }
  ##
  cl <- parallel::makeCluster(ncl, setup_strategy = "sequential")
  doParallel::registerDoParallel(cl)
  
  # data simplification
  xsfc <- st_geometry(x)
  kgeom <- matrix(unlist(xsfc), ncol = 2, byrow = TRUE)
  
  v <- as.matrix(x = x[, var, drop = TRUE])
  
  ysfc <- st_geometry(y)
  
  # sequence to split unknowpts
  ny <- nrow(y)
  sequence <- unique(c(seq(1, ny, size), ny + 1))
  lseq <- length(sequence) - 1
  
  # split unknownpts and put it on a list
  ml <- list()
  for  (i in 1:lseq) {
    ml[[i]] <- ysfc[(sequence[i]):(sequence[i + 1] - 1)]
  }
  
  # dispatch
  pot <- foreach::`%dopar%`(
    foreach::foreach(
      ysfc = ml,
      .packages = "sf",
      .combine = c,
      .inorder = FALSE
    ),
    {
      # FUNS
      eucledian_simple <- function(from, to) {
        sqrt((from[1] - to[1])^2 + (from[2] - to[2])^2)
      }
      if (fun == "e") {
        alpha <- log(2) / span^beta
        fric <- function(alpha, matdist, beta) {
          exp(-alpha * matdist^beta)
        }
      }
      if (fun == "p") {
        alpha <- (2^(1 / beta) - 1) / span
        fric <- function(alpha, matdist, beta) {
          (1 + alpha * matdist)^(-beta)
        }
      }
      
      # Buffer limit
      gbuf <- st_buffer(ysfc, limit)
      inter <- st_intersects(gbuf, xsfc, prepared = TRUE)
      
      # data transformation
      ugeom <- matrix(unlist(ysfc), ncol = 2, byrow = TRUE)
      
      # go through each y
      l <- vector("list", nrow(ugeom))
      for (i in seq_along(l)) {
        kindex <- unlist(inter[i])
        kn <- kgeom[kindex, , drop = FALSE]
        un <- ugeom[i, ]
        matdist <- apply(kn, 1, eucledian_simple, un)
        un <- apply(
          X = v[kindex, , drop = FALSE],
          MARGIN = 2,
          FUN = function(x) {
            sum(x * fric(alpha, matdist, beta), na.rm = TRUE)
          }
        )
        l[[i]] <- un
      }
      unlist(l)
    }
  )
  # stop parralel
  parallel::stopCluster(cl)
  if (length(var) == 1) {
    pot <- as.numeric(pot)
  } else {
    pot <- matrix(pot,
                  ncol = length(var), byrow = TRUE,
                  dimnames = list(NULL, var)
    )
  }
  
  return(pot)
}