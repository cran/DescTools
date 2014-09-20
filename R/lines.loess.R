lines.loess <-
function(x, col = getOption("col1", hblue), lwd = 2, lty = "solid", type = "l",  n = 100
                             , conf.level = 0.95, args.band = NULL, ...){
  
  newx <- seq(from = min(x$x, na.rm=TRUE), to = max(x$x, na.rm=TRUE), length = n)
  fit <- predict(x, newdata=newx, se = !is.na(conf.level) )
  
  if (!is.na(conf.level)) {
    
    # define default arguments for ci.band
    args.band1 <- list(col = SetAlpha(col, 0.30), border = NA) 
    # override default arguments with user defined ones
    if (!is.null(args.band)) args.band1[names(args.band)] <- args.band 
    
    # add a confidence band before plotting the smoother
    lwr.ci <- fit$fit + fit$se.fit * qnorm((1 - conf.level)/2)
    upr.ci <- fit$fit - fit$se.fit * qnorm((1 - conf.level)/2)
    do.call("DrawBand", c(args.band1, list(x=c(newx, rev(newx))), list(y=c(lwr.ci, rev(upr.ci)))) ) 
    # reset fit for plotting line afterwards
    fit <- fit$fit
  }
  
  lines( y = fit, x = newx, col = col, lwd = lwd, lty = lty, type = type)
  
}
