ScheffeTest.aov <-
function(x, which=NULL, contrasts = NULL, conf.level=0.95, ...){
  
  mm <- model.tables(x, "means")
  if (is.null(mm$n)) 
    stop("no factors in the fitted model")
  tabs <- mm$tables[-1L]
  
  if(is.null(which)) which <- seq_along(tabs)
  
  tabs <- tabs[which]
  nn <- mm$n[names(tabs)]
  nn_na <- is.na(nn)
  if (all(nn_na)) 
    stop("'which' specified no factors")
  if (any(nn_na)) {
    warning("'which' specified some non-factors which will be dropped")
    tabs <- tabs[!nn_na]
    nn <- nn[!nn_na]
  }
  out <- setNames(vector("list", length(tabs)), names(tabs))
  MSE <- sum(x$residuals^2)/x$df.residual
  
  autoContr <- is.null(contrasts)
  if(!is.null(contrasts)){
    contrasts <- data.frame(contrasts)
  }
  
  # nm <- "tension"
  for (nm in names(tabs)) {
    tab <- tabs[[nm]]
    means <- as.vector(tab)
    
    nms <- if (length(d <- dim(tab)) > 1L) {
      dn <- dimnames(tab)
      apply(do.call("expand.grid", dn), 1L, paste, collapse = ":")
    } else names(tab)
    
    n <- nn[[nm]]
    if (length(n) < length(means)) 
      n <- rep.int(n, length(means))
    
    if(autoContr) contrasts <- Contrasts(nms)
    
    psi <- apply(contrasts * means, 2, sum)
    sscoeff <- apply(contrasts * contrasts / n, 2, sum)
    mspsi <- (psi * psi) / sscoeff
    
# Korrektur von Daniel Wollschlaeger 9.9.2014:
#     psi <- contrasts %*% means
#     sscoeff <- contrasts * contrasts %*% (1/n)
    
    dferr <- x$df.residual
    dfgrp <- length(x$residuals) - dferr - 1
    
    pval <- pf(psi^2/(MSE*sscoeff*dfgrp), 
               df1=dfgrp, df2=dferr, lower.tail=FALSE)
    
    critvalue <- dfgrp * qf(1-conf.level, dfgrp, dferr, lower.tail=FALSE)
    
    lwr <- psi - sqrt(critvalue) * sqrt(MSE * sscoeff) 
    upr <- psi + sqrt(critvalue) * sqrt(MSE * sscoeff) 
    
    out[[nm]] <- cbind(diff=psi, lwr, upr, pval)
    colnames(out[[nm]]) <- c("diff","lwr.ci","upr.ci","pval")
    
    if(!autoContr) {
      # define contrasts rownames
      rownames(out[[nm]]) <-  apply(contrasts, 2, function(x) 
        gettextf("%s-%s", paste(nms[x>0], collapse=","), 
                 paste(nms[x<0], collapse=",")) )
      if(is.na(conf.level)) out[[nm]] <- out[[nm]][,-c(2:3)]
    }  
    
    if(autoContr & is.na(conf.level)) {
      out[[nm]] <- matrix(NA, nrow=length(means), ncol=length(means))
      out[[nm]][lower.tri(out[[nm]], diag = FALSE)] <- pval
      dimnames(out[[nm]]) <- list(nms, nms)
      out[[nm]] <- out[[nm]][-1, -ncol(out[[nm]])]
    } 

  }
  
  class(out) <- c("PostHocTest")
  attr(out, "orig.call") <- x$call
  attr(out, "conf.level") <- conf.level
  attr(out, "ordered") <- FALSE
  attr(out, "method") <- "Scheffe Test"
  attr(out, "method.str") <- gettextf("\n  Posthoc multiple comparisons of means : %s \n", attr(out, "method"))


  return(out)
  
}
