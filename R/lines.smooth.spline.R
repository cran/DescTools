lines.smooth.spline <-
function (x, col = getOption("col1", hblue), lwd = 2, lty = "solid", 
                                 type = "l", conf.level = 0.95, args.band = NULL, 
                                 ...) {
  
  # newx <- seq(from = min(x$x, na.rm = TRUE), to = max(x$x, na.rm = TRUE), length = n)
  newx <- x$x
  
  fit <- predict(x, newdata = newx)
  
  if (!is.na(conf.level)) {
    args.band1 <- list(col = SetAlpha(col, 0.3), border = NA)
    if (!is.null(args.band)) 
      args.band1[names(args.band)] <- args.band
    
    res <- (x$yin - x$y)/(1-x$lev)      # jackknife residuals
    sigma <- sqrt(var(res))                     # estimate sd
    upr.ci <- fit$y + qnorm((1 - conf.level)/2) * sigma * sqrt(x$lev)   # upper 95% conf. band
    lwr.ci <- fit$y - qnorm((1 - conf.level)/2) * sigma * sqrt(x$lev)   # lower 95% conf. band
    
    do.call("DrawBand", c(args.band1, list(x = c(newx, rev(newx))), 
                          list(y = c(lwr.ci, rev(upr.ci)))))
    
  }
  
  lines(y = fit$y, x = fit$x, col = col, lwd = lwd, lty = lty, type = type)
}
