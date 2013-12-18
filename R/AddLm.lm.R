AddLm.lm <-
function (x, col = "blue", lwd = 2, lty = "solid", 
                      type = "l", n = 100, conf.level = 0.95, args.cband = NULL, 
                      pred.level = NA, args.pband = NULL, ...) {
  
  mod <- x$model
  
  # we take simply the second column of the model data.frame to identify the x variable
  # this will crash, if there are several resps and yield nonsense if there is 
  # more than one pred,
  # so check for a simple regression model y ~ x (just one resp, just one pred)
  
  newx <- data.frame(seq(from = min(mod[,2], na.rm = TRUE), to = max(mod[,2], 
                                                                     na.rm = TRUE), length = n))
  colnames(newx) <- names(mod)[2]
  fit <- predict(x, newdata = newx)
  
  if (!(is.na(pred.level) || identical(args.pband, NA)) ) {
    args.pband1 <- list(col = SetAlpha(col, 0.12), border = NA)
    if (!is.null(args.pband)) 
      args.pband1[names(args.pband)] <- args.pband
    
    ci <- predict(x, interval="prediction", newdata=newx, level=pred.level) # Vorhersageband
    do.call("DrawBand", c(args.pband1, list(x = c(unlist(newx), rev(unlist(newx)))), 
                          list(y = c(ci[,2], rev(ci[,3])))))
  }
  
  if (!(is.na(conf.level) || identical(args.cband, NA)) ) {
    args.cband1 <- list(col = SetAlpha(col, 0.12), border = NA)
    if (!is.null(args.cband)) 
      args.cband1[names(args.cband)] <- args.cband
    
    ci <- predict(x, interval="confidence", newdata=newx, level=conf.level) # Vertrauensband
    do.call("DrawBand", c(args.cband1, list(x = c(unlist(newx), rev(unlist(newx)))), 
                          list(y = c(ci[,2], rev(ci[,3])))))
  }
  
  lines(y = fit, x = unlist(newx), col = col, lwd = lwd, lty = lty, 
        type = type)
}
