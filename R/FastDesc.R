



Desc <- function (x, ..., wrd = NULL) {

  if (is.null(wrd))
    UseMethod("Desc")
  else
    UseMethod("DescWrd")

}


Desc.numeric <- function (x, main = NULL, highlow = TRUE, digits = NULL,
                              plotit = getOption("plotit", FALSE), ...) {

  if (!any(class(x) %in% c("numeric", "integer")))
    stop(gettextf("x must be numeric and has class%s %s",
                  ifelse(length(class(x)) > 1, "es:", ""), paste(class(x),
                                                                 collapse = ", ")))

  .stats <- function(x){

    probs= c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)

    ntot <- length(x) # total count

    x <- na.omit(x)
    n <- length(x)    # now without NAs

    NAs <- ntot - n   # number of NAs

    if(n > 0){

      # the quantiles, totally analogue to the core of stats::quantile:
      index <- 1 + (n - 1) * probs
      lo <- floor(index)
      hi <- ceiling(index)
      x <- sort(x, partial = unique(c(lo, hi)))
      qs <- x[lo]
      i <- which(index > lo)
      h <- (index - lo)[i]
      qs[i] <- (1 - h) * qs[i] + h * x[hi[i]]
      names(qs) <- c("min", ".05", ".10", ".25", "median", ".75", ".90", ".95", "max")

      # ... here we go, all we need so far is in qs

      # proceed with the parameteric stuff
      meanx <- mean.default(x)      # somewhat faster than mean

      # we send the SORTED vector WITHOUT NAs to the C++ function to calc the power sum(s)
      psum <- .Call("DescTools_n_pow_sum", PACKAGE = "DescTools", x, meanx)

      # this is method 3 in the usual functions Skew and Kurt
      skewx <- ((1/n * psum$sum3) /  (psum$sum2 / n)^1.5) * ((n - 1)/n)^(3/2)
      kurtx <- ((((1/n * psum$sum4) /  (psum$sum2 / n)^2) - 3)  + 3) * (1 - 1/n)^2 - 3

      sdx <- (psum$sum2 / (n-1))^0.5

  # changed for 0.99.12: leave out the test (as Werner Stahel advised against)
  #     if (n %[]% c(5, 5000)) {
  #       tres <- tryCatch(shapiro.test(x), error = function(e) { e })
  #     } else {
  #       tres <- tryCatch(AndersonDarlingTest(x, null = "pnorm"), error = function(e) { e })
  #     }

  #     vals <- prettyNum(c(psum$small_val, psum$large_val), big.mark = "'")
  #     frq <- c(psum$small_freq, psum$large_freq)
  #     frqtxt <- paste(" (", Format(frq, fmt=.fmt_abs()), ")", sep = "")
  #     frqtxt[frq < 2] <- ""
  #     txt <- StrTrim(paste(vals, frqtxt, sep = ""))
  #     lowtxt <- paste(head(txt, min(length(psum$small_val), nlow)), collapse = ", ")
  #     hightxt <- paste(rev(tail(txt, min(length(psum$large_val), nhigh))), collapse = ", ")

      # put together the results
      res <- list(length=ntot, n=n, NAs=NAs, unique=psum$unique, "0s"=psum$zero,
                  mean=meanx, meanSE = sdx/sqrt(n),
                  quant=qs,
                  range=unname(diff(qs[c(1,9)])),
                  sd=sdx, vcoef=sdx/meanx,
                  mad=mad(x, center = qs[5]), IQR=unname(diff(qs[c(4,6)])),
                  skew=skewx, kurt=kurtx,
                  small=data.frame(val=psum$small_val, freq=psum$small_freq),
                  large=data.frame(val=psum$large_val, freq=psum$large_freq)
      )


    } else {

      # put together the results
      res <- list(length=ntot, n=n, NAs=NAs, unique=0, "0s"=0,
                  mean=NA, meanSE = NA,
                  quant=setNames(rep(NA,9), c("min", ".05", ".10", ".25", "median", ".75", ".90", ".95", "max")),
                  range=NA,
                  sd=NA, vcoef=NA,
                  mad=NA, IQR=NA,
                  skew=NA, kurt=NA,
                  small=data.frame(val=NA, freq=NA),
                  large=data.frame(val=NA, freq=NA)
      )

    }

    return(res)

  }

  nlow <- 5
  nhigh <- 5

  defdigits <- is.null(digits)

  z <- .stats(x)

  cat(paste(rep("-", (as.numeric(options("width")) - 2)), collapse = ""), "\n")

  if (is.null(main))
    main <- gettextf("%s (%s)", deparse(substitute(x)), paste(class(x), collapse = ", "))
  if (!identical(main, NA))
    cat(main)
  if (!is.null(attr(x, "label")))
    cat(" :", strwrap(attr(x, "label"), indent = 2, exdent = 2), sep = "\n")
  cat("\n\n")

  zz <- z
  zz[c(1:5)] <- lapply(zz[c(1:5)], Format, fmt=.fmt_abs())
  if(defdigits){
    # how many digits do we want to use?
    # we would use the same number as quantile does...
    out <- capture.output(z$quant)
    digits <- max(2, MaxDigits(strsplit(StrTrim(out[[2]]), split=" ")[[1]][1]))
    # for counts the quants would tipically return 0 digits, mean and ds deserve some though
    # if(digits==0) digits <- 1

  }

  zz[8] <- lapply(zz[8], Format, fmt=.fmt_num(digits))
  # we killed the names..
  names(zz[["quant"]]) <- names(z[["quant"]])
  zz[c(6:7, 9:15)] <- lapply(zz[c(6:7, 9:15)], Format, fmt=.fmt_num(digits))

  lst <- list(l1 = unlist(zz[c(1:7)]), l2=zz[["quant"]][-c(1,9)], l3=unlist(zz[c(9:15)]))

  width <- max(c(unlist(lapply(lst, nchar)), unlist(lapply(lapply(lst, names), nchar))), na.rm=TRUE)
  if (z$unique == z$n)
    lst$l1["unique"] <- "= n"

  cat(paste(lapply(lst, .txtline, width = width, ind = "  ",
                   space = "  "), collapse = "\n"), "\n")
  # clarify: print.gap can be set with space, which is set here to 2 spaces
  # should we make an argument out of that?

  # we need to do that even if highlow == FALSE, as Desc.integer could need the result!!
  if(class(x) == "numeric"){
    vals <- Format(c(z$small$val, z$large$val), fmt=.fmt_num(digits=digits))
  } else {
    vals <- Format(c(z$small$val, z$large$val), fmt=.fmt_abs())
  }
  # we don't want too many digits but as well no trailing 0s by default
  # if(defdigits) vals <- gsub("\\.$", "\\.0", gsub("0+$", "", vals))
  if(defdigits) vals <- gsub("\\.0+$", "\\.0", gsub("^(\\d+\\.\\d*?[1-9])0+$", "\\1", vals, perl = TRUE))

  if(z$unique < 10 && z$n > 0) {
    # we table the frequencies if there's less than 10 unique values

    ff <- Sort(merge(data.frame(z$small$val, z$small$freq), data.frame(z$large$val, z$large$freq),
                     by.x = 1, by.y = 1, all.x=TRUE, all.y=TRUE))
    ff$n <- do.call(Coalesce, ff[, c(2,3)])
    tab <- as.table(ff$n)
    names(tab) <- signif(ff[,1])
    f <- Freq(tab)

    z$lowtxt <- NA
    z$hightxt <- NA
    z$freq <- f

  } else{
    frq <- c(z$small$freq, z$large$freq)
    frqtxt <- paste(" (", Format(frq, fmt=.fmt_abs()), ")", sep = "")
    frqtxt[frq < 2] <- ""
    txt <- StrTrim(paste(vals, frqtxt, sep = ""))
    z$lowtxt <- paste(head(txt, min(length(z$small$val), nlow)), collapse = ", ")
    z$hightxt <- paste(rev(tail(txt, min(length(z$large$val), nhigh))), collapse = ", ")
    z$freq <- NA
  }

  if (highlow) {
    if(identical(z$freq, NA)){
      cat(paste("lowest : ", z$lowtxt, "\n", "highest: ", z$hightxt, "\n\n", sep = ""))
    } else {
      cat("\n")
      print(f)
      cat("\n")
    }
  }

  if (plotit)
    if (z$n > 0)
      PlotDesc.numeric(x, main = main)
    else
      cat(gettextf("Nothing to plot in %s\n\n", deparse(substitute(x))))

  invisible(z)

}



Desc.factor <- function (x, main = NULL, ord = c("desc", "asc", "name", "level"),
                             maxrows = 12, digits = NULL, plotit = getOption("plotit", FALSE),
                             ...) {

  # calculate ***********************
  .stats <- function(x, ord, digits){

    nan <- length(x)
    x <- na.omit(x)
    n <- length(x)

    if(n > 0){
      f <- Freq(x, ord=ord)
      res <- list(length=nan, n=n,
                  NAs = nan - n, levels=nlevels(x),
                  unique=sum(f$freq>0), dupes=any(f$freq>1),
                  frq = f
      )
    } else {
      res <- list(length=nan, n=n,
                  NAs = nan - n, levels=0,
                  unique=0, dupes=0,
                  frq = NA
      )
    }

    return(res)
  }


  # print ***********************
  if(!is.null(digits)){
    opt <- options(digits = digits)
    on.exit(options(opt))
  }
  ord <- match.arg(arg = ord, choices = c("desc", "asc", "name", "level"))
  if (is.null(main))
    main <- gettextf("%s (%s)", deparse(substitute(x)), paste(class(x),
                                                              collapse = ", "))
  if (nlevels(x) == 2) {
    Desc.logical(x, main = gsub(pattern = "factor)", replacement = "factor - dichotomous)",
                                x = main), digits=digits, plotit = plotit)
    return(invisible())
  }

  cat(paste(rep("-", (as.numeric(options("width")) - 2)), collapse = ""), "\n")
  if (!identical(main, NA))
    cat(main)
  if (!is.null(attr(x, "label")))
    cat(" :", strwrap(attr(x, "label"), indent = 2, exdent = 2), sep = "\n")

  z <- .stats(x, ord=ord, digits=digits)

  lfmt <- lapply(z[c("length","n","NAs","levels","unique","dupes")], .fmt)
  lfmt$dupes <- ifelse(lfmt$dupes == "0", "n", "y")
  width <- max(c(unlist(lapply(lfmt, nchar)), unlist(lapply(names(lfmt),
                                                            nchar))))
  cat("\n\n")
  cat(paste(.txtline(lfmt, width = width, ind = "  ", space = " "),
            collapse = "\n"))

  if(z$n > 0){   # print that only if there's real data
    if (is.na(maxrows)) {
      maxrows <- nrow(z$frq)
    }
    if (maxrows < 1) {
      maxrows <- sum(z$frq[, 5] < maxrows) + 1
    }
    lfmt$frq <- z$frq[1:min(nrow(z$frq), maxrows), ]
    txt.frq <- .CaptOut(lfmt$frq)
  #   txt1.frq <- paste(substr(txt.frq, 0, regexpr("level", txt.frq[1]) + 5),
  #                         x = substr(txt.frq, regexpr("level", txt.frq[1]) + 5 + 1,
  #                                    nchar(txt.frq[1])),
  #                    sep = "")
    cat("\n")
    cat(txt.frq, sep = "\n")
    if (maxrows < z$levels)
      cat("... etc.\n [list output truncated]\n\n")
    else cat("\n")
  }

  if (plotit)
    if (z$n > 0)
      PlotDesc.factor(x, main = main, maxrows = maxrows, ord = ord, ecdf=TRUE, ...)
  else
    cat(gettextf("\nNothing to plot in %s\n\n", deparse(substitute(x))))


  invisible(z)

}


Desc.ordered <- function (x, main = NULL, ...) {

  if (is.null(main))
    main <- gettextf("%s (%s)", deparse(substitute(x)),
                     paste(class(x), collapse = ", "))

  if (is.null(list(...)$ord)) {
    Desc.factor(x, main = main, ord = "level", ...)
  }
  else {
    Desc.factor(x, main = main, ...)
  }
  invisible()
}


Desc.character <- function (x, main = NULL, ...) {

  if (is.null(main))
    main <- gettextf("%s (%s)", deparse(substitute(x)),
                     paste(class(x), collapse = ", "))
  Desc.ordered(x = factor(x), main = main, ...)
  invisible()
}


Desc.logical <- function (x, main = NULL, digits = NULL, conf.level = 0.95,
                          plotit = getOption("plotit", FALSE), ...) {

  # calculate ***********************
  .stats <- function(x){

    f <- table(x, useNA = "always")
    idx <- !is.na(names(f))
    NAs <- unname(f[!idx])
    n <- sum(f[idx])
    bf <- BinomCI(f[idx], n)

    res <- list(length=sum(f), n=n,
                NAs = NAs,
                unique=length(f)-1,
                afrq=f[idx], rfrq = bf
    )

    return(res)
  }


  # print ***********************

  if (is.null(main))
    main <- gettextf("%s (%s)", deparse(substitute(x)), paste(class(x),
                                                              collapse = ", "))
  cat(paste(rep("-", (as.numeric(options("width")) - 2)), collapse = ""),
      "\n")
  if (!identical(main, NA))
    cat(main)
  if (!is.null(attr(x, "label")))
    cat(" :", strwrap(attr(x, "label"), indent = 2, exdent = 2),
        sep = "\n")

  z <- .stats(x)

  lfmt <- lapply(z[c("length","n","NAs","unique")], .fmt)
  width <- max(c(unlist(lapply(lfmt, nchar)), unlist(lapply(names(lfmt),
                                                            nchar))))
  cat("\n\n")
  cat(paste(.txtline(lfmt, width = width, ind = "  ", space = " "),
            collapse = "\n"), "\n")

  if(z$n > 0) {
    out <- cbind(
             freq=Format(z$afrq, fmt = .fmt_abs()),
                  Format(z$rfrq, fmt = .fmt_per()))

    rownames(out) <- rownames(z$afrq)
    colnames(out) <- c("freq", "perc",
                       gettextf(c("lci%s", "uci%s"),
                                Format(conf.level, digits=2, leading="drop")))

    txt <- capture.output(print(out, quote=FALSE, right=TRUE, print.gap=2))
    cat(paste(txt[1], Coalesce(getOption("footnote1"), "'"),
              sep = ""), txt[-1], sep = "\n")
    cat(gettextf("\n%s %s%s-CI Wilson\n\n", Coalesce(getOption("footnote1"),
                                                     "'"), conf.level * 100, "%"))
  }

  if (plotit)
    if (z$n > 0)
      PlotDesc.logical(x, main = main)
  else
    cat(gettextf("Nothing to plot in %s\n\n", deparse(substitute(x))))


  invisible(z)

}


Desc.integer <- function (x, main = NULL, maxrows = 12, freq = NULL, digits = NULL,
          plotit = getOption("plotit", FALSE), ...) {

  if (is.null(main))
    main <- gettextf("%s (%s)", deparse(substitute(x)),
                     paste(class(x), collapse = ", "))

  uvals <- length(unique(na.omit(x)))

  if (is.null(freq))
    freq <- uvals <= 12
  if (uvals == 2) {
    lres <- Desc.logical(x, main = gsub(pattern = "integer)",
                                        replacement = "integer - dichotomous)", x = main),
                         plotit = FALSE)
  } else {
    lres <- Desc.numeric(x, main = main, highlow = FALSE, digits=digits, plotit = FALSE)

    if (lres$n > 0) {
      if (freq) {
        lres$frq <- Freq(x = as.factor(x))
#         opt <- options(digits = digits)
        txt.frq <- .CaptOut(print(lres$frq, digits=digits))
        # options(opt)
#         txt.frq <- paste(substr(txt.frq, 0, regexpr("level", txt.frq[1]) + 5),
#                          gsub(pattern = "0\\.", replacement = " \\.",
#                               x = substr(txt.frq, regexpr("level", txt.frq[1]) + 5 + 1,
#                                          nchar(txt.frq[1]))), sep = "")
#         txt.frq <- paste(substr(txt.frq, 0, regexpr("level", txt.frq[1]) + 5),
#                               x = substr(txt.frq, regexpr("level", txt.frq[1]) + 5 + 1,
#                                          nchar(txt.frq[1])), sep = "")
        cat("\n")
        cat(txt.frq[1:min((maxrows + 1), length(txt.frq))],
            sep = "\n")
        if (maxrows < uvals)
          cat("... etc.\n [list output truncated]\n\n")
        else cat("\n")

      }
      else {
        cat(paste("lowest : ", lres$lowtxt, "\n", "highest: ", lres$hightxt, "\n\n", sep = ""))
      }
    }
  }


  if (plotit)
    if (lres$n > 0)
      PlotDesc.integer(x, main = main, maxrows = maxrows)
  else
    cat(gettextf("Nothing to plot in %s\n\n", deparse(substitute(x))))


  invisible(lres)
}



Desc.Date <- function(x, main = NULL, maxrows = 10, digits = NULL, dprobs = NULL, mprobs=NULL, plotit=getOption("plotit", FALSE), ... ) {
  # time aggregation already in the definition of the variable:
  # example:     cut( x, breaks="quarter" )
  #              breaks: day, month, quarter, year

  opt <- options(digts=digits); on.exit(options(opt))

  if( is.null(main)) main <- gettextf("%s (%s)", deparse(substitute(x)), paste(class(x), collapse=", "))

  cat( paste(rep("-",(as.numeric(options("width"))-2)), collapse=""), "\n" )
  if(!identical(main, NA))  cat( main )
  if( !is.null(attr(x,"label")) ) cat(" :", strwrap(attr(x,"label"), indent=2, exdent=2), sep="\n" )

  # format values according to defined pretty nums
  x <- na.omit(x)

  nan <-length(attr(x, "na.action"))
  n <- length(x)

  lfmt <- lapply( list("length"=n + nan, "n"=n, "NAs"=nan, unique=length(unique(x)) ), .fmt )
  # what's max width in names and formatted values?
  width <- max( c( unlist(lapply(lfmt, nchar)), unlist(lapply(names(lfmt), nchar))))

  cat( "\n\n")
  cat( paste(.txtline(lfmt, width=width, ind="  ", space=" "), collapse="\n" ), "\n")

  if(n==nan) return()

  cat(HighLow(x, nlow=4, na.rm=FALSE), "\n", sep="") # set na.rm to FALSE as we have already omitted NAs

  # weekdays in your current locale, Sunday : Saturday
  dtab <- table(Weekday(x, fmt="ddd"))
  cat("\nWeekdays:\n")
  if(is.null(dprobs)) dprobs <- rep(1/7, 7)
  cat(capture.output(
    Desc.table(dtab, p=dprobs, expected=FALSE, stdres=TRUE, plotit=FALSE)
    )[-c(1:5)], sep="\n")  # get rid of the first 5 lines of description

  # months in your current locale
  mtab <- table(Month(x, fmt="mmm"))
  cat("\nMonths:\n")
  if(is.null(mprobs)) mprobs <- c(31,28,31,30,31,30,31,31,30,31,30,31) / 365
  cat(capture.output(
    Desc.table(mtab, p=mprobs, expected=FALSE, stdres=TRUE, plotit=FALSE)
  )[-c(1:5)], sep="\n")    # get rid of the first 5 lines of description

  tspan <- diff(range(x, na.rm=TRUE)) /15  # set na.rm=TRUE as we inform user about NAs
  hbreaks <- switch( findInterval( tspan, c(0,5,30,100,350,1250,3500,35000) )
                     , "1" = "days"
                     , "2" = "weeks"
                     , "3" = "months"
                     , "4" = "quarters"
                     , "5" = "years"
                     , "6" =  # 5-years"
                       as.Date(seq( from=ISOdate( as.integer(min(format(x,"%Y"),na.rm=TRUE)) %/% 5 * 5,1,1)
                                    , to=ISOdate( (as.integer(max(format(x,"%Y"),na.rm=TRUE))) %/% 5 * 5 + 5, 1, 1)
                                    , "5 years"
                       ))
                     , "7" =  # 10-years"
                       as.Date(seq( from=ISOdate( as.integer(min(format(x,"%Y")),na.rm=TRUE) %/% 10 * 10,1,1)
                                    , to=ISOdate( (as.integer(max(format(x,"%Y"),na.rm=TRUE))) %/% 10 * 10 + 10, 1, 1)
                                    , "10 years"
                       ))
  )

  if(!is.null(hbreaks)){
    cat("\nBy", hbreaks, ":\n\n")
    print(Freq(x=x, breaks=hbreaks))

  } else {
    cat("Warning:\n  No plausible breaks for years found!\n")
  }
  cat("\n")

  if(plotit) PlotDesc.Date(x, main=main)
  invisible()

}




Desc.data.frame <- function (x,
                                 sep = paste(rep("-", (as.numeric(options("width")) - 2)), collapse = ""),
                                 main = NULL, enum = TRUE, ...) {

  cat("\n", sep, "\n", sep = "")
  cat(.CaptOut(Str(x, list.len = Inf)), sep = "\n")
  cat("\n")

  Desc(x=as.list(x), sep=sep, main=main, enum=enum, summary=FALSE, ...)

}


Desc.list <- function (x,
                           sep = paste(rep("-", (as.numeric(options("width")) - 2)), collapse = ""),
                           main = NULL, enum = TRUE, summary = TRUE, ...) {

  if(summary){
    cat("\n", sep, "\n", sep = "")
    cat(.CaptOut(Str(x, list.len = Inf)), sep = "\n")
    cat("\n")
  }

  if (is.null(main))
    main <- paste(if (enum)
      paste(seq_along(names(x)), "- "), names(x),
      " (", lapply(lapply(x, class), paste, collapse = ", "),
      ")", sep = "")
  else main <- rep(main, length.out = ncol(x))

  for (i in 1:length(x)) {
    Desc(x[[i]], main = main[i], ...)
  }

  cat("\n")
  invisible()
}


Desc.default <- function (x, ...) {

  if (!is.null(class(x))) {
    cat(gettextf("\nSorry, don't know how to handle class(es) %s (%s)!\n\n",
                 paste(class(x), collapse = ", "), deparse(substitute(x))))

  } else {
    cat(gettextf("\nObject %s does not exist!\n\n", deparse(substitute(x))))
  }
}






