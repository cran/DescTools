PlotBagPairs <-
function(dm, trim = 0.0, main, numeric.only = TRUE, 
                         factor = 3, approx.limit = 300, pch = 16, 
                         cex = 0.8, precision = 1, col.loophull = "#aaccff",
                         col.looppoints = "#3355ff", col.baghull = "#7799ff",
                         col.bagpoints = "#000088", ...){
  if(missing(main)) main <- paste(deparse(substitute(dm)),"/ trim =",round(trim,3))
  if(length(trim) == 1) trim <- rep(trim, ncol(dm))
  if(numeric.only){
    dm <- dm[, idx <- sapply(1:ncol(dm), function(x) is.numeric(dm[,x]))]
    trim <- trim[idx]
  }
  for(j in 1:ncol(dm)){
    x <- dm[,j]
    if(!is.numeric(x)) x <- as.numeric(x)
    if( trim[j] > 0) {
      na.idx <- is.na(x)      
      xlim <- quantile(x[!na.idx], c(trim[j] , 1-trim[j])) 
      x[ na.idx |  x < xlim[1] | xlim[2] < x ] <- NA
    }
    dm[,j] <- x
  }
  # DM0 <<- dm
  h.fn <- function(x,y){
    idx <- !is.na(x) & !is.na(y)
    x <- x[ idx ]; y <- y[ idx ]
    BP <- PlotBag(x,y,add=TRUE,factor = factor, approx.limit = approx.limit, pch = pch, 
                  cex = cex, precision = precision, col.loophull = col.loophull,
                  col.looppoints = col.looppoints, col.baghull = col.baghull,
                  col.bagpoints = col.bagpoints, verbose=FALSE)
    # BP <<- BP ### for debugging
  }
  par(mfrow=c(1,1))
  pairs(dm, panel = h.fn, ...) 
  mtext(main, line=2.5)
  dm
}
