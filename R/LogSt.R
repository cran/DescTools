LogSt <-
function(x, calib = x, threshold = NULL, mult = 1) {

# original function logst in source regr
#   
#   # Purpose:   logs of x, zeros and small values treated well
#   # *********************************************************************
#   # Author: Werner Stahel, Date:  3 Nov 2001, 08:22
#   x <- cbind(x)
#   calib <- cbind(calib)
#   lncol <- ncol(calib)
#   ljthr <- length(threshold) > 0
#   if (ljthr) {
#     if (!length(threshold) %in% c(1, lncol))
#       stop("!LogSt! length of argument 'threshold' is inadequate")
#     lthr <- rep(threshold, length=lncol)
#     ljdt <- !is.na(lthr)
#   } else {
#     ljdt <- rep(TRUE, lncol)
#     lthr <- rep(NA, lncol)
#     for (lj in 1:lncol) {
#       lcal <- calib[, lj]
#       ldp <- lcal[lcal > 0 & !is.na(lcal)]
#       if(length(ldp) == 0) ljdt[lj] <- FALSE else {
#         lq <- quantile(ldp,probs = c(0.25,0.75), na.rm = TRUE)
#         if(lq[1] == lq[2]) lq[1] <- lq[2]/2
#         lthr[lj] <- lc <- lq[1]^(1 + mult) / lq[2]^mult
#       }
#     }
#   }
#   # transform x
#   for (lj in 1:lncol) {
#     ldt <- x[,lj]
#     lc <- lthr[lj]
#     li <- which(ldt < lc)
#     if (length(li))
#       ldt[li] <- lc * 10^((ldt[li] - lc) / (lc * log(10)))
#     x[,lj] <- log10(ldt)
#   }
#   if (length(colnames(x)))
#     lnmpd <- names(ljdt) <- names(lthr) <- colnames(x)  else
#     lnmpd <- as.character(1:lncol)
# 
#   attr(x,"threshold") <- c(lthr)
# 
#   if (any(!ljdt)) {
#     warning(':LogSt: no positive x for variables',lnmpd[!ljdt],
#             '. These are not transformed')
#     attr(x,"untransformed") <- c(ljdt)
#   }
#   x

  
  if(is.null(threshold)){
    lq <- quantile(calib[calib > 0], probs = c(0.25, 0.75), na.rm = TRUE)
    if (lq[1] == lq[2]) lq[1] <- lq[2]/2
    threshold <- lq[1]^(1 + mult)/lq[2]^mult
  }
  
  res <- rep(NA, length(x))
  idx <- (x < threshold)
  idx.na <- is.na(idx)
  res[idx & !idx.na] <- log10(threshold) + ((x[idx & !idx.na] - threshold)/(threshold * log(10)))
  res[!idx & !idx.na] <- log10(x[!idx & !idx.na])
  
  attr(res, "threshold") <- threshold
  return(res)
  
}
