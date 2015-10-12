



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
        txt.frq <- .CaptOut(print(lres$frq, digits=digits))
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



Desc.matrix <- function(x, main=NULL, rfrq = NULL, margins = c(1,2), p = rep(1/length(x), length(x)), conf.level = 0.95,
                        verbose = c("medium","low","high"), plotit=getOption("plotit", FALSE), ... ){
  Desc.table(x, main=main, rfrq = rfrq, margins = margins, p = p, plotit = plotit, verbose = verbose, conf.level = conf.level, ... )
  invisible()
}


Desc.table <- function(x, main=NULL, rfrq = NULL, margins = c(1,2), p = rep(1/length(x), length(x)), conf.level = 0.95,
                       verbose = c("medium","low","high"), plotit=getOption("plotit", FALSE), ... ){

  .warn <- function(x){
    if(any(x$expected < 5) && is.finite(x$parameter)){
      cat("\nWarning message:\n  Exp. counts < 5: Chi-squared approx. may be incorrect!!\n")
    }
  }

  opt  <- options(scipen=4); on.exit(options(opt))

  # define verbosity
  verbose <- match.arg(verbose, c("medium","low","high"))
  verbose <- match(verbose, c("low","medium","high"), nomatch=2)

  if(is.null(rfrq)) rfrq <- ifelse(verbose > 1, "111","000")

  if(is.null(main)) main <- gettextf("%s (%s)", deparse(substitute(x)), paste(class(x), collapse=", "))

  if(!identical(main, NA)) {
    cat( paste(rep("-",(as.numeric(options("width"))-2)), collapse=""), "\n" )
    cat(main)
  }
  if( !is.null(attr(x,"label")) ) cat(" :", strwrap(attr(x,"label"), indent=2, exdent=2), sep="\n" )
  cat("\n")

  # Pairs summary
  n <- sum(x)

  if(length(dim(x)) > 2) { # multdim table

    cat("\nSummary: \n",
        "n: ", n, ", ", length(dim(x)), "-dim table: ", paste(dim(x), collapse=" x ")
        , "\n\n", sep="" )

    print(ftable(addmargins(x, c(1, length(dim(x))))))
    cat("\n")

  } else {  # <= 2-dimensional table

    if(length(dim(x))==1) {       # 1-dim table ****
      cat("\nSummary: \n",
          "n: ", n,
          ", rows: ", dim(x)[1]
          , "\n\n", sep="" )

      suppressWarnings(r.chisq <- chisq.test(x, p=p))
      cat("Pearson's Chi-squared test (1-dim uniform):\n  "
          , .CaptOut(r.chisq)[5], "\n\n", sep="")
      .warn(r.chisq)

      # use Freq instead of PercTable for 1-d tables
      f <- Freq(x)
      f[,2] <- Format(f[,2], fmt=.fmt_abs())
      f[,3] <- Format(f[,3], fmt=.fmt_per())
      f[,4] <- Format(f[,4], fmt=.fmt_abs())
      f[,5] <- Format(f[,5], fmt=.fmt_per())

      if(InDots(..., arg="expected", default=FALSE))
        f <- cbind(f, exp=Format(unname(r.chisq$expected), big.mark="'", digits=1))
      if(InDots(..., arg="residuals", default=FALSE))
        f <- cbind(f, res=Format(unname(as.matrix(r.chisq$residuals)), digits=1, big.mark="'"))
      if(InDots(..., arg="stdres", default=FALSE))
        f <- cbind(f, stdres=Format(unname(as.matrix(r.chisq$stdres)), digits=1))

      print.data.frame(f, print.gap=2)

    } else {                      # 2-dim tabl *****

      if(!is.null(attr(x, "missings")))
        missn <- paste(",", attr(x, "missings"), paste="")
      else
        missn <- ""

      cat("\nSummary: \n",
          "n: ", Format(n, fmt=.fmt_abs()),
          ", rows: ", Format(dim(x)[1], fmt=.fmt_abs()),
          ", columns: ", Format(dim(x)[2], fmt=.fmt_abs()),
          missn
          , "\n\n", sep="" )

      if(dim(x)[1] == 2 & dim(x)[2] == 2 ){

        if(verbose=="3"){
          suppressWarnings(r.chisq <- chisq.test(x, p=p, correct = FALSE))
          cat("Pearson's Chi-squared test:\n  ", .CaptOut(r.chisq)[5], "\n", sep="")
        }
        suppressWarnings(r.chisq <- chisq.test(x, p=p))
        cat("Pearson's Chi-squared test (cont. adj):\n  ", .CaptOut(r.chisq)[5], "\n", sep="")
        cat("Fisher's exact test ", .CaptOut( fisher.test(x))[5], "\n", sep="")

        if(verbose > 1){ # print only with verbosity > 1
          cat("", .CaptOut( mcnemar.test(x))[5], "\n", sep="")
        }

        .warn(r.chisq)

        if(verbose > 1){ # print only with verbosity > 1
          cat("\n")
          m <- ftable(format(rbind(
            "odds ratio    " = OddsRatio(x, conf.level=conf.level)
            , "rel. risk (col1)  " = RelRisk(x, conf.level=conf.level, method="wald", delta=0)
            , "rel. risk (col2)  " = RelRisk(x[,c(2,1)], conf.level=conf.level, method="wald", delta=0)
          ), digits=3, nsmall=3))

          attr(m, "col.vars")[[1]][1] <- "estimate"
          txt <- capture.output(print(m))
          txt[1] <- paste(txt[1], getOption("footnote1"), sep="")
          cat(txt, sep="\n")
          cat("\n")
        }

      } else {

        # we report chisquare without cont-corr for rxc and with cont-corr for 2x2 by default
        suppressWarnings(r.chisq <- chisq.test(x, p=p, correct = FALSE))
        cat("Pearson's Chi-squared test:\n  ", .CaptOut(r.chisq)[5], "\n", sep="")

        if(verbose=="3"){
          suppressWarnings(r.chisq <- chisq.test(x, p=p))
          cat("Pearson's Chi-squared test (cont. adj):\n  ", .CaptOut(r.chisq)[5], "\n", sep="")
        }

        if(verbose > 1){ # print only with verbosity > 1

          # Log-likelihood chi-squared (G2) test of independence (homogeneity)
          lhrat <- 2 * sum(r.chisq$observed * log(r.chisq$observed/r.chisq$expected), na.rm=TRUE)
          alpha <- pchisq(lhrat, df=r.chisq$parameter, lower.tail = FALSE)
          cat(gettextf("Likelihood Ratio:\n  X-squared = %s, df = %s, p-value = %s\n",
                       round(lhrat, 4), r.chisq$parameter, format.pval(alpha, digits=4)))
          # Mantel-Haenszel ChiSquared (linear hypothesis)
          mh <- MHChisqTest(x)
          alpha <- mh$p.value
          cat(gettextf("Mantel-Haenszel Chi-squared:\n  X-squared = %s, df = %s, p-value = %s\n",
                       round(mh$statistic, 4), 1, format.pval(alpha, digits=4)))

        }

        .warn(r.chisq)

      }

      switch(verbose
             , "1" = { cat("\n")
             }
             , "2" = {
               cat(sprintf(
                 "\nPhi-Coefficient        %.3f\nContingency Coeff.     %.3f\nCramer's V             %.3f\n"
                 , Phi(x)
                 , ContCoef(x)
                 , CramerV(x)
               ) )
               cat("\n")
             }
             , "3" = {
               cat("\n")
               txt <- capture.output(Assocs(x, conf.level = conf.level))
               txt[1] <- paste(txt[1], getOption("footnote1"), sep="")
               cat(txt, sep="\n")
               cat("\n")
             }
      )

      print(PercTable(x, rfrq=rfrq, margins=margins, ...))

      if(verbose=="3" || (dim(x)[1]==2 & dim(x)[2]==2))
        cat(gettextf("\n----------\n%s %s%s conf. level\n", getOption("footnote1"), conf.level*100, "%"))

    }

    cat("\n")

  }

  if(plotit) {
    horiz <- InDots(..., arg="horiz", default=TRUE)
    PlotDesc.table(x, main=main, horiz = horiz)
  }
  invisible()

}



Desc.Lc <- function (x, main = NULL, p = c(0.8,0.9,0.95,0.99), plotit=getOption("plotit", FALSE), ...) {

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
  txt <- .CaptOut(txt)
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




Desc.flags <- function(x, i=1, plotit=getOption("plotit", FALSE), ...){

  cat( paste(rep("-",(as.numeric(options("width"))-2)), collapse=""), "\n" )
  main <- InDots(..., arg="main", default = "Multiple dichotomous variables")
  cat(main)
  if( !is.null(attr(x,"label")) ) cat(" :", strwrap(attr(x,"label"), indent=2, exdent=2), sep="\n" )
  cat("\n")

  cat( "\nSummary: \n", "total n: ", nrow(x), "\n\n", sep="" )

  d.sub <- x

  flags <- do.call(rbind, lapply(d.sub, function(z) {
    tab <- table(z)
    data.frame(val = names(tab[i]), abs=tab[i], BinomCI(tab[i], sum(tab)))
  }
  ))
  out <- data.frame( do.call(rbind,  lapply(d.sub, function(x) cbind(NAs=sum(is.na(x)), n=length(x)- sum(is.na(x)))))
                     , flags)
  out[,5:7] <- apply(out[,5:7],2, Format, digits=3)

  print(out, quote=FALSE)
  cat("\n")

  if(plotit) PlotDesc(x)

}



Desc.formula <- function(formula, data = parent.frame(), subset, plotit=getOption("plotit", FALSE), ...) {

  mf <- match.call(expand.dots = FALSE)

  # parse dots.arguments, such as not to send unappropriate arguments to subfunctions
  dotargs.factor.factor <- mf$...[ names(mf$...)[
    !is.na(match(names(mf$...), names( formals( Desc.table) )))
    ] ]
  dotargs.numeric.factor <- mf$...[ names(mf$...)[
    !is.na(match(names(mf$...), names( formals( DescNumFact) )))
    ] ]
  dotargs.factor.numeric <- mf$...[ names(mf$...)[
    !is.na(match(names(mf$...), names( formals( DescFactNum) )))
    ] ]
  dotargs.numeric.numeric <- mf$...[ names(mf$...)[
    !is.na(match(names(mf$...), names( formals( DescNumNum) )))
    ] ]

  subset.expr <- mf$subset
  mf$subset <- NULL
  if (!missing(subset)) {
    s <- eval(subset.expr, data, parent.frame())
    data <- data[s,]
  }

  mm <- ParseFormula(formula=formula, data=data)

  # don't want AsIs (will come in case of I(...)) to proceed, so just coerce to vector an back again
  # but don't use the following, as interaction names will be set to y.x instead of y:x
  # mm$lhs$mf.eval <- data.frame(lapply(mm$lhs$mf.eval, as.vector))
  # mm$rhs$mf.eval <- data.frame(lapply(mm$rhs$mf.eval, as.vector))
  for(i in which(lapply(mm$lhs$mf.eval, class) == "AsIs")) {
    mm$lhs$mf.eval[,i] <- as.vector(mm$lhs$mf.eval[,i])
  }
  for(i in which(lapply(mm$rhs$mf.eval, class) == "AsIs")) {
    mm$rhs$mf.eval[,i] <- as.vector(mm$rhs$mf.eval[,i])
  }

  # start output
  cat("\nCall:\n")
  cat(paste(deparse(sys.call()), sep = "\n", collapse = "\n"),"\n\n", sep = "")

  # start analysis
  for(resp in mm$lhs$vars){         # for all response variables
    for(pred in mm$rhs$vars){       # evalutate for all conditions
      x <- mm$lhs$mf.eval[,resp]
      grp <- mm$rhs$mf.eval[,pred]

      cat( paste(rep("-",(as.numeric(options("width"))-2)), collapse=""), "\n" )
      cat( paste(resp, " ~ ", pred, sep="") )
      if( !is.null(attr(x,"label")) ) cat(" :", strwrap(attr(x,"label"), indent=2, exdent=2), sep="\n" )
      cat("\n")

      # coerce logicals and characters to factors
      #       if( class(x)[1] %in% c("logical","character")) x <- factor(x)
      #       if( class(grp)[1] %in% c("logical","character")) grp <- factor(grp)
      if( IsDichotomous(x)) x <- factor(x)
      if( IsDichotomous(grp)) grp <- factor(grp)


      if(class(x)[1] %in% c("numeric","integer")){

        if(class(grp)[1] %in% c("numeric","integer")){
          do.call( DescNumNum, args=append( list(x=grp, y=x, xname=pred, yname=resp, plotit=plotit), dotargs.numeric.numeric))

        } else if(class(grp)[1] %in% c("factor","ordered")){
          do.call( DescNumFact, args=append( list(x=x, grp=grp, xname=resp, grpname=pred, plotit=plotit), dotargs.numeric.factor ))

        } else {
          cat(gettextf("Don't know how to describe class: %s ~ %s!\n", paste(class(x), collapse=", "),
                       paste(class(grp), collapse=", ")), "\n")
        }
      } else if(class(x)[1] %in% c("factor","ordered")){

        if( class(grp)[1] %in% c("numeric","integer")){
          do.call( DescFactNum, args=append( list(x=x, y=grp, xname=resp, yname=pred, plotit=plotit), dotargs.factor.numeric ))

        } else if ( class(grp)[1] %in% c("factor","ordered")){
          useNA <- InDots(..., arg="useNA", default="no")
          tab <- table(x, grp, dnn=c(resp, pred), useNA=useNA)

          if(useNA == "no"){  # add missing info only if there are any (depends on useNA)
            n <- max(length(x),length(grp))
            idcomp <- complete.cases(x, grp)  # cases
            vn <- sum(idcomp)                 # valid n pairs
            missn <- paste("total pairs: ", Format(n, fmt=.fmt_abs()),
                           #                          ", valid: ", .fmt(vn), " (", round(vn/n*100, 3), "%)",
                           #                           ... well, don't use .fmt_per() here, as it would not be understandable in the context
                           ", missings: ", Format(n-vn, fmt=.fmt_abs()), " (", Format((n-vn)/n, digits=2, fmt="%"), ")",
                           sep="")
            attr(tab, "missings") <- missn
          }

          main <- gettextf("%s ~ %s", resp, pred)
          do.call( Desc, args=append( list(x=tab, xname="", grpname="", plotit=FALSE, main=NA), dotargs.factor.factor) )
          if(plotit) PlotDesc.table(tab, main=main)
        } else {
          cat(gettextf("Don't know how to describe class: %s ~ %s!\n", class(x), class(grp)), "\n")
        }
      } else {
        cat(gettextf("Don't know how to describe class: %s ~ %s!\n", class(x), class(grp)), "\n")
      }

    }
  }
  invisible()

}





