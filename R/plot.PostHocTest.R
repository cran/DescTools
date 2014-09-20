plot.PostHocTest <-
function(x, ...){
  # original:   stats:::plot.TukeyHSD(x, ...)
  
  for (i in seq_along(x)) {
    xi <- x[[i]][, -4L, drop = FALSE]
    yvals <- nrow(xi):1L
    dev.hold()
    on.exit(dev.flush())
    plot(c(xi[, "lwr.ci"], xi[, "upr.ci"]), rep.int(yvals, 2L), 
         type = "n", axes = FALSE, xlab = "", ylab = "", main = NULL, 
         ...)
    axis(1, ...)
    axis(2, at = nrow(xi):1, labels = dimnames(xi)[[1L]], 
         srt = 0, ...)
    abline(h = yvals, lty = 1, lwd = 0.5, col = "lightgray")
    abline(v = 0, lty = 2, lwd = 0.5, ...)
    segments(xi[, "lwr.ci"], yvals, xi[, "upr.ci"], yvals, ...)
    segments(as.vector(xi), rep.int(yvals - 0.1, 3L), as.vector(xi), 
             rep.int(yvals + 0.1, 3L), ...)
    title(main = paste0(format(100 * attr(x, "conf.level"), 
                               digits = 2L), "% family-wise confidence level\n"), 
          xlab = paste("Differences in mean levels of", names(x)[i]))
    box()
    dev.flush()
    on.exit()
  }
  
}
