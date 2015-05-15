CorPolychor <-
function (x, y, ML=FALSE, control=list(), std.err=FALSE, maxcor=.9999){
    
    # last modified 21 Oct 08 by J. Fox

    binBvn <- function(rho, row.cuts, col.cuts, bins=4){  
      # last modified 29 Mar 07 by J. Fox
      
      row.cuts <- if (missing(row.cuts)) c(-Inf, 1:(bins - 1)/bins, Inf) else  c(-Inf, row.cuts, Inf)
      col.cuts <- if (missing(col.cuts)) c(-Inf, 1:(bins - 1)/bins, Inf) else  c(-Inf, col.cuts, Inf)
      r <- length(row.cuts) - 1
      c <- length(col.cuts) - 1
      P <- matrix(0, r, c)
      R <- matrix(c(1, rho, rho, 1), 2, 2)
      for (i in 1:r){
        for (j in 1:c){
          P[i,j] <- pmvnorm(lower=c(row.cuts[i], col.cuts[j]),
                            upper=c(row.cuts[i+1], col.cuts[j+1]),
                            corr=R)
        }
      }
      P
    }
    
    
    f <- function(pars) {
      if (length(pars) == 1){
        rho <- pars
        if (abs(rho) > maxcor) rho <- sign(rho)*maxcor
        row.cuts <- rc
        col.cuts <- cc
      }
      else {
        rho <- pars[1]
        if (abs(rho) > maxcor) rho <- sign(rho)*maxcor
        row.cuts <- pars[2:r]
        col.cuts <- pars[(r+1):(r+c-1)]
      }
      P <- binBvn(rho, row.cuts, col.cuts)
      - sum(tab * log(P))
    }
    
    tab <- if (missing(y)) x else table(x, y)
    zerorows <- apply(tab, 1, function(x) all(x == 0))
    zerocols <- apply(tab, 2, function(x) all(x == 0))
    zr <- sum(zerorows)
    if (0 < zr) warning(paste(zr, " row", suffix <- if(zr == 1) "" else "s",
                              " with zero marginal", suffix," removed", sep=""))
    zc <- sum(zerocols)
    if (0 < zc) warning(paste(zc, " column", suffix <- if(zc == 1) "" else "s",
                              " with zero marginal", suffix, " removed", sep=""))
    tab <- tab[!zerorows, ,drop=FALSE]  
    tab <- tab[, !zerocols, drop=FALSE] 
    r <- nrow(tab)
    c <- ncol(tab)
    if (r < 2) {
      warning("the table has fewer than 2 rows")
      return(NA)
    }
    if (c < 2) {
      warning("the table has fewer than 2 columns")
      return(NA)
    }
    n <- sum(tab)
    rc <- qnorm(cumsum(rowSums(tab))/n)[-r]
    cc <- qnorm(cumsum(colSums(tab))/n)[-c]
    if (ML) {
      result <- optim(c(optimise(f, interval=c(-1, 1))$minimum, rc, cc), f,
                      control=control, hessian=std.err)
      if (result$par[1] > 1){
        result$par[1] <- 1
        warning("inadmissible correlation set to 1")
      }
      else if (result$par[1] < -1){
        result$par[1] <- -1
        warning("inadmissible correlation set to -1")
      }
      if (std.err) {
        chisq <- 2*(result$value + sum(tab * log((tab + 1e-6)/n)))
        df <- length(tab) - r - c
        result <- list(type="polychoric",
                       rho=result$par[1],
                       row.cuts=result$par[2:r],
                       col.cuts=result$par[(r+1):(r+c-1)],
                       var=solve(result$hessian),
                       n=n,
                       chisq=chisq,
                       df=df,
                       ML=TRUE)
        class(result) <- "polycor"
        return(result)
      }
      else return(as.vector(result$par[1]))
    }
    else if (std.err){
      result <- optim(0, f, control=control, hessian=TRUE, method="BFGS")
      if (result$par > 1){
        result$par <- 1
        warning("inadmissible correlation set to 1")
      }
      else if (result$par < -1){
        result$par <- -1
        warning("inadmissible correlation set to -1")
      }
      chisq <- 2*(result$value + sum(tab *log((tab + 1e-6)/n)))
      df <- length(tab) - r - c 
      result <- list(type="polychoric",
                     rho=result$par,
                     var=1/result$hessian,
                     n=n,
                     chisq=chisq,
                     df=df,
                     ML=FALSE)
      class(result) <- "CorPolychor"
      return(result)
    }
    else optimise(f, interval=c(-1, 1))$minimum
  }
