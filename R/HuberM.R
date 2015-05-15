HuberM <-
function(x, k = 1.5, mu = median(x), s = mad(x, center=mu), 
    na.rm = FALSE, conf.level = NA, ci.type = c("wald", "boot"), ...){
  
  # new interface to HuberM, making it less complex
  # refer to robustbase::huberM if more control is required
  
  ## Originally from  /u/ftp/NDK/Source-NDK-9/R/rg2-fkt.R :
  tauHuber <- function(x, mu, k=1.5, s = mad(x), resid = (x - mu)/s) {
    ## Purpose: Korrekturfaktor Tau fuer die Varianz von Huber-M-Schaetzern
    ## -------------------------------------------------------------------------
    ## Arguments: x = Daten mu = Lokations-Punkt k = Parameter der Huber Psi-Funktion
    ## -------------------------------------------------------------------------
    ## Author: Rene Locher Update: R. Frisullo 23.4.02;  M.Maechler (as.log(); s, resid)
    inr <- abs(resid) <= k
    psi  <- ifelse(inr, resid, sign(resid)*k)                # psi (x)
    psiP <- as.logical(inr)# = ifelse(abs(resid) <= k, 1, 0) # psi'(x)
    length(x) * sum(psi^2) / sum(psiP)^2
  }
  
  wgt.himedian <- function(x, weights = rep(1,n)) {
    
    # Purpose: weighted hiMedian of x
    # Author: Martin Maechler, Date: 14 Mar 2002
    n <- length(x <- as.double(x))
    stopifnot(storage.mode(weights) %in% c("integer", "double"))
    if(n != length(weights))
      stop("'weights' must have same length as 'x'")
    # if(is.integer(weights)) message("using integer weights")
    .C(if(is.integer(weights)) "wgt_himed_i" else "wgt_himed",
       x, n, weights,
       res = double(1))$res
  }
  
  
  ##  A modified "safe" (and more general) Huber estimator:
  .huberM <-
    function(x, k = 1.5, weights = NULL,
             tol = 1e-06,
             mu = if(is.null(weights)) median(x) else wgt.himedian(x, weights),
             s = if(is.null(weights)) mad(x, center=mu)
             else wgt.himedian(abs(x - mu), weights),
             se = FALSE,
             warn0scale = getOption("verbose"))
    {
      ## Author: Martin Maechler, Date: 6 Jan 2003, ff
      
      ## implicit 'na.rm = TRUE':
      if(any(i <- is.na(x))) {
        x <- x[!i]
        if(!is.null(weights)) weights <- weights[!i]
      }
      n <- length(x)
      sum.w <-
        if(!is.null(weights)) {
          stopifnot(is.numeric(weights), weights >= 0, length(weights) == n)
          sum(weights)
        } else n
      it <- 0L
      NA. <- NA_real_
      if(sum.w == 0) # e.g 'x' was all NA
        return(list(mu = NA., s = NA., it = it, se = NA.)) # instead of error
      
      if(se && !is.null(weights))
        stop("Std.error computation not yet available for the case of 'weights'")
      if (s <= 0) {
        if(s < 0) stop("negative scale 's'")
        if(warn0scale && n > 1)
          warning("scale 's' is zero -- returning initial 'mu'")
      }
      else {
        wsum <- if(is.null(weights)) sum else function(u) sum(u * weights)
        repeat {
          it <- it + 1L
          y <- pmin(pmax(mu - k * s, x), mu + k * s)
          mu1 <- wsum(y) / sum.w
          if (abs(mu - mu1) < tol * s)
            break
          mu <- mu1
        }
      }
      list(mu = mu, s = s, it = it,
           SE = if(se) s * sqrt(tauHuber(x, mu=mu, s=s, k=k) / n) else NA.)
    }
  
  
  # begin main
  
  if(na.rm) x <- na.omit(x)
  if(anyNA(x)) return(NA)
  
  
  if(is.na(conf.level)){
    res <- .huberM(x=x, k=k, mu=mu, s=s, warn0scale=TRUE)$mu
    
    return(res)
    
  } else { 
    
    switch(match.arg(ci.type)
           ,"wald"={ 
             res <- .huberM(x=x, k=k, mu=mu, s=s, se=TRUE, warn0scale=TRUE)
             ci <- qnorm(1-(1-conf.level)/2) * res$SE
             res <- c(hm=res$mu, lwr.ci=res$mu - ci, upr.ci=res$mu + ci)
           }
           ,"boot" ={
             R <- InDots(..., arg="R", default=1000)
             ci.type <- InDots(..., arg="ci.type", default="perc")
             
             boot.hm <- boot(x, function(x, d){
               hm <- .huberM(x=x[d], k=k, mu=mu, s=s, se=TRUE)
               return(c(hm$mu, hm$SE^2))
               }, R=R, ...)
             ci <- boot.ci(boot.hm, conf=conf.level, type=ci.type)
             
             if(ci.type =="norm") {
               lwr.ci <- ci[[4]][2]
               upr.ci <- ci[[4]][3]
             } else {
               lwr.ci <- ci[[4]][4]
               upr.ci <- ci[[4]][5]
             }  
             
             res <- c(hm=boot.hm$t0[1], lwr.ci=lwr.ci, upr.ci=upr.ci)

           }
           )
    return(res)

  }

}
