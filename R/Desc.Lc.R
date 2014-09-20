Desc.Lc <-
function (x, main = NULL, p = c(0.8,0.9,0.95,0.99), plotit=getOption("plotit", FALSE), ...) {

  # describe a Lorenz curve
  
  if (is.null(main)) 
    main <- gettextf("%s (%s)", deparse(substitute(x)), 
                      paste(class(x), collapse = ", "))
  n <- length(x$p)
  
  cat(paste(rep("-", (as.numeric(options("width")) - 2)), collapse = ""), 
      "\n")
  if (!identical(main, NA)) 
    cat(main)
  if (!is.null(attr(x, "label"))) 
    cat(" :", strwrap(attr(x, "label"), indent = 2, exdent = 2), 
        sep = "\n")
  cat("\n\n")
  
  d.frm <- data.frame( (1-p), sapply(p, function(p){1 - approx(x=x$p, y=x$L, xout=p)$y}))
  colnames(d.frm) <- c("1-p","1-L(p)")
  
  txt <- d.frm
  txt[,2] <- round(txt[,2], 3)
  txt <- capture.output(txt)
  txt <- gsub(pattern = "0\\.", replacement = " \\.", txt)
  
  lres <- list(l1 = list(n = n, `0s` = max(x$p[x$L==0]), Gini=x$Gini))
  
  lfmt <- lapply(lres, lapply, .fmt)
  width <- max(c(unlist(lapply(lfmt, nchar)), unlist(lapply(lapply(lfmt, names), nchar))))
  cat(paste(lapply(lfmt, .txtline, width = width, ind = "  ", 
                   space = " "), collapse = "\n"), "\n")
  
  cat(txt, sep = "\n")
  
  if (plotit) 
    if (lres$l1$n > 0) 
      plot(x, main = main)
  
  invisible(lres)
}
