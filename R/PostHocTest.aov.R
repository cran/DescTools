PostHocTest.aov <-
function (x, which = NULL, 
                         method=c("hsd","bonf","lsd","scheffe","newmankeuls"),
                         conf.level = 0.95, ordered = FALSE, ...) {
  
  method <- match.arg(method)
  
  if(method=="scheffe"){
    out <- ScheffeTest(x=x, which=which, conf.level=conf.level, ...)
    
  } else {
    
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
    for (nm in names(tabs)) {
      tab <- tabs[[nm]]
      means <- as.vector(tab)
      nms <- if (length(d <- dim(tab)) > 1L) {
        dn <- dimnames(tab)
        apply(do.call("expand.grid", dn), 1L, paste, collapse = ":")
      }
      else names(tab)
      n <- nn[[nm]]
      if (length(n) < length(means)) 
        n <- rep.int(n, length(means))
      
      # this will be ignored for bonferroni, lsd
      if (method %in% c("hsd","newmankeuls") & as.logical(ordered)) {
        ord <- order(means)
        means <- means[ord]
        n <- n[ord]
        if (!is.null(nms)) 
          nms <- nms[ord]
      }
      
      center <- outer(means, means, "-")
      keep <- lower.tri(center)
      center <- center[keep]
      
      switch(method
             ,"bonf" = {     
               width <-  qt(1 - (1 - conf.level)/(length(means) * (length(means) - 1)), x$df.residual) * 
                 sqrt(MSE * outer(1/n, 1/n, "+"))[keep]
               est <- center/sqrt(MSE * outer(1/n, 1/n, "+")[keep])
               
               pvals <- pmin(2 * pt(abs(est), df = x$df.residual, lower.tail = FALSE) 
                             * ((length(means)^2 - length(means))/2), 1)
               method.str <- "Bonferroni"
               
             } 
             ,"lsd" = {
               width <-  qt(1 - (1 - conf.level)/2, x$df.residual) * 
                 sqrt(MSE * outer(1/n, 1/n, "+"))[keep]
               est <- center/sqrt(MSE * outer(1/n, 1/n, "+")[keep])
               pvals <- 2 * pt(abs(est), df = x$df.residual, lower.tail = FALSE)
               method.str <- "Fisher LSD"
             }
             ,"hsd" = {
               width <- qtukey(conf.level, length(means), x$df.residual) * 
                 sqrt((MSE/2) * outer(1/n, 1/n, "+"))[keep]
               est <- center/(sqrt((MSE/2) * outer(1/n, 1/n, "+"))[keep])
               pvals <- ptukey(abs(est), length(means), x$df.residual, 
                               lower.tail = FALSE)
               method.str <- "Tukey HSD"
               
             }
             ,"newmankeuls" ={
               nmean <- (abs(outer(rank(means), rank(means), "-")) + 1)[keep]
               
               width <- qtukey(conf.level, nmean, x$df.residual) * 
                 sqrt((MSE/2) * outer(1/n, 1/n, "+"))[keep]
               
               est <- center/(sqrt((MSE/2) * outer(1/n, 1/n, "+"))[keep])
               
               pvals <- ptukey(abs(est), nmean, x$df.residual, lower.tail = FALSE)
               method.str <- "Newman-Keuls"
               
             }
             ,"dunnet" = {
               method.str <- "Dunnet"
             }
      )    
      
      if(!is.na(conf.level)){
        dnames <- list(NULL, c("diff", "lwr.ci", "upr.ci", "pval"))
        if (!is.null(nms)) 
          dnames[[1L]] <- outer(nms, nms, paste, sep = "-")[keep]
        out[[nm]] <- array(c(center, center - width, 
                             center + width, pvals), c(length(width), 4L), dnames)
      } else {
        out[[nm]] <- matrix(NA, nrow=length(means), ncol=length(means))
        out[[nm]][lower.tri(out[[nm]], diag = FALSE)] <- pvals
        dimnames(out[[nm]]) <- list(nms, nms)
        out[[nm]] <- out[[nm]][-1, -ncol(out[[nm]])]
        
      }
    }
    
    class(out) <- c("PostHocTest")
    attr(out, "orig.call") <- x$call
    attr(out, "conf.level") <- conf.level
    attr(out, "ordered") <- ordered
    attr(out, "method") <- method.str
  }
  
  return(out)
  
}
