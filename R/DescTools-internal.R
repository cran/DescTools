.__global__ <-
c("d.units", "d.periodic", "d.prefix", "day.name", "day.abb", 
"wdConst", "hblue", "hred", "hgreen")
.CaptOut <-
function(..., file = NULL, append = FALSE, width=150) {
  
  opt <- options(width=width)
  
  args <- substitute(list(...))[-1L]
  rval <- NULL
  closeit <- TRUE
  if (is.null(file)) 
    file <- textConnection("rval", "w", local = TRUE)
  else if (is.character(file)) 
    file <- file(file, if (append) 
      "a"
      else "w")
  else if (inherits(file, "connection")) {
    if (!isOpen(file)) 
      open(file, if (append) 
        "a"
        else "w")
    else closeit <- FALSE
  }
  else stop("'file' must be NULL, a character string or a connection")
  sink(file)
  on.exit({
    sink()
    if (closeit) close(file)
    options(opt)
  })
  pf <- parent.frame()
  evalVis <- function(expr) withVisible(eval(expr, pf))
  for (i in seq_along(args)) {
    expr <- args[[i]]
    tmp <- switch(mode(expr), expression = lapply(expr, evalVis), 
                  call = , name = list(evalVis(expr)), stop("bad argument"))
    for (item in tmp) if (item$visible) 
      print(item$value)
  }
  on.exit(options(opt))
  sink()
  if (closeit) 
    close(file)
  if (is.null(rval)) 
    invisible(NULL)
  else rval
  
}
.DoCount <-
function(y, x, wts) {

  # O(n log n):
  # http://www.listserv.uga.edu/cgi-bin/wa?A2=ind0506d&L=sas-l&P=30503
  
  
  btree <- function(n) {
    ranks <- rep(0L, n)
    yet.to.do <- 1:n
    depth <- floor(logb(n, 2))
    start <- as.integer(2^depth)
    lastrow.length <- 1 + n - start
    indx <- seq(1L, by = 2L, length = lastrow.length)
    ranks[yet.to.do[indx]] <- start + 0:(length(indx) - 1L)
    yet.to.do <- yet.to.do[-indx]
    while (start > 1) {
      start <- as.integer(start/2)
      indx <- seq(1L, by = 2L, length = start)
      ranks[yet.to.do[indx]] <- start + 0:(start - 1L)
      yet.to.do <- yet.to.do[-indx]
    }
    ranks
  }
  
  if(missing(wts)) wts <- rep(1, length(x))
  
  ord <- order(y)
  ux <- sort(unique(x))
  n2 <- length(ux)
  index <- btree(n2)[match(x[ord], ux)] - 1L
  y <- cbind(y,1)
  res <- .Call("conc", y[ord,], as.double(wts[ord]), 
        as.integer(index), as.integer(n2))
  
  return(list(pi.c = NA, pi.d = NA, C = res[2], D = res[1]))
  
}
.fmt <-
function(x, digits=3, sdigits=7, big.mark="'") {
  x <- as.numeric(x)
  if(is.finite(x)){
    fdigits <- ifelse( IsWhole(x), 0
      , ifelse(x > 1e3, digits - (round(log(abs(x),10),0)-3), digits )
    )
    switch( findInterval( abs(x), c(0,.Machine$double.eps^0.5, 1e-4, 1e6) )
                    , "1" = { formatC(x, digits=fdigits, format="f", big.mark=big.mark ) }
                    , "2" = { formatC(x, digits=digits, format="e", big.mark=big.mark ) }
                    , "3" = { formatC(x, digits=fdigits, format="f", big.mark=big.mark ) }
                    , "4" = { formatC(x, digits=digits, format="e", big.mark=big.mark ) }
    )
  } else {
    formatC(x)
  }  
}
.ImportSPSS <-
function(datasetname = "dataset") {
# read.spss
# function (file, use.value.labels = TRUE, to.data.frame = FALSE, 
#           max.value.labels = Inf, trim.factor.names = FALSE, trim_values = TRUE, 
#           reencode = NA, use.missings = to.data.frame) 
  e1 <- environment()
  env.dsname <- character()
  env.use.value.labels <- logical()
  env.to.data.frame <- logical()
  env.max.value.labels <- character()
  env.trim.factor.names <- logical()
  env.trim.values <- logical()
  env.reencode <- character()
  env.use.missings <- logical()
  lst <- NULL
  
  OnOK <- function() {
    assign("lst", list(), envir = e1)
    assign("env.dsname", tcltk::tclvalue(dsname), envir = e1)
    assign("env.use.value.labels", tcltk::tclvalue(use.value.labels), envir = e1)
    assign("env.to.data.frame", tcltk::tclvalue(to.data.frame), envir = e1)
    assign("env.max.value.labels", tcltk::tclvalue(max.value.labels), envir = e1)
    assign("env.trim.factor.names", tcltk::tclvalue(trim.factor.names), envir = e1)
    assign("env.trim.values", tcltk::tclvalue(trim.values), envir = e1)
    assign("env.reencode", tcltk::tclvalue(reencode), envir = e1)
    assign("env.use.missings", tcltk::tclvalue(use.missings), envir = e1)
    tcltk::tkdestroy(top)
  }
  
  top <- .InitDlg(350, 300, main="Import SPSS Dataset")
  
  dsname <- tcltk::tclVar(datasetname)
  dsnameFrame <- tcltk::tkframe(top, padx = 10, pady = 10)
  entryDsname <- tcltk::ttkentry(dsnameFrame, width=30, textvariable=dsname)
  
  optionsFrame <- tcltk::tkframe(top, padx = 10, pady = 10)
  
  use.value.labels <- tcltk::tclVar("1")
  use.value.labelsCheckBox <- tcltk::ttkcheckbutton(optionsFrame, text="Use value labels", variable=use.value.labels)

  to.data.frame <- tcltk::tclVar("1")
  to.data.frameCheckBox <- tcltk::ttkcheckbutton(optionsFrame, 
                                            text="Convert value labels to factor levels", variable=to.data.frame)
  max.value.labels <- tcltk::tclVar("Inf")
  entryMaxValueLabels <- tcltk::ttkentry(optionsFrame, width=30, textvariable=max.value.labels)
  
  trim.values <- tcltk::tclVar("1")
  trim.valuesCheckBox <- tcltk::ttkcheckbutton(optionsFrame, text="Ignore trailing spaces when matching"
                                         , variable=trim.values)
  trim.factor.names <- tcltk::tclVar("1")
  trim.factor.namesCheckBox <- tcltk::ttkcheckbutton(optionsFrame, text="Trim trailing spaces from factor levels"
                                               , variable=trim.factor.names)
  reencode <- tcltk::tclVar("")
  entryReencode <- tcltk::ttkentry(optionsFrame, width=30, textvariable=reencode)

  use.missings <- tcltk::tclVar("1")
  use.missingsCheckBox <- tcltk::ttkcheckbutton(optionsFrame, text="Use missings", 
                                                variable=use.missings)
  
  tcltk::tkgrid(tcltk::tklabel(dsnameFrame, text="Enter name for data set:  "), entryDsname, sticky="w")
  tcltk::tkgrid(dsnameFrame, columnspan=2, sticky="w")
  tcltk::tkgrid(use.value.labelsCheckBox, sticky="w")
  tcltk::tkgrid(to.data.frameCheckBox, sticky="nw")
  tcltk::tkgrid(tcltk::ttklabel(optionsFrame, text="Maximal value label:"), sticky="nw")
  tcltk::tkgrid(entryMaxValueLabels, padx=20, sticky="nw")
  tcltk::tkgrid(trim.valuesCheckBox, sticky="w")
  tcltk::tkgrid(trim.factor.namesCheckBox, sticky="w")
  tcltk::tkgrid(tcltk::ttklabel(optionsFrame, text="Reencode character strings to the current locale:"), sticky="nw")
  tcltk::tkgrid(entryReencode, padx=20, sticky="nw")
  tcltk::tkgrid(use.missingsCheckBox, sticky="w")
  tcltk::tkgrid(optionsFrame, sticky="w")
  
  buttonsFrame <- tcltk::tkframe(top, padx = 10, pady = 10)
  tfButOK <- tcltk::tkbutton(buttonsFrame, text = "OK", command = OnOK, width=10)
  tfButCanc <- tcltk::tkbutton(buttonsFrame, width=10, text = "Cancel", command = function() tcltk::tkdestroy(top))
  
  tcltk::tkgrid(tfButOK, tfButCanc)
  tcltk::tkgrid.configure(tfButCanc, padx=c(6,6))
  tcltk::tkgrid.columnconfigure(buttonsFrame, 0, weight=2)
  tcltk::tkgrid.columnconfigure(buttonsFrame, 1, weight=1)
  
  tcltk::tkgrid(buttonsFrame, sticky="ew")
  tcltk::tkwait.window(top)
  
  if(!is.null(lst)){
    lst <- list(dsname=env.dsname, use.value.labels=as.numeric(env.use.value.labels), 
                to.data.frame=as.numeric(env.to.data.frame), 
                max.value.labels=env.max.value.labels, trim.factor.names=as.numeric(env.trim.factor.names),
                trim.values=as.numeric(env.trim.values), reencode=env.reencode, use.missings=as.numeric(env.use.missings)  )
  }
  return(lst)
  
}
.ImportStataDlg <-
function(datasetname = "dataset") {
  
#   function (file, convert.dates = TRUE, convert.factors = TRUE, 
#             missing.type = FALSE, convert.underscore = FALSE, warn.missing.labels = TRUE) 

  e1 <- environment()
  env.dsname <- character()
  env.convert.dates <- logical()
  env.convert.factors <- logical()
  env.convert.underscore <- logical()
  env.missing.type <- logical()
  env.warn.missing.labels <- logical()
  lst <- NULL
  
  OnOK <- function() {
    assign("lst", list(), envir = e1)
    assign("env.dsname", tcltk::tclvalue(dsname), envir = e1)
    assign("env.convert.dates", tcltk::tclvalue(convert.dates), envir = e1)
    assign("env.convert.factors", tcltk::tclvalue(convert.factors), envir = e1)
    assign("env.convert.underscore", tcltk::tclvalue(convert.underscore), envir = e1)
    assign("env.missing.type", tcltk::tclvalue(missing.type), envir = e1)
    assign("env.warn.missing.labels", tcltk::tclvalue(warn.missing.labels), envir = e1)
    tcltk::tkdestroy(top)
  }
  
  top <- .InitDlg(350, 220, main="Import Stata Dataset")
  
  dsname <- tcltk::tclVar(datasetname)
  dsnameFrame <- tcltk::tkframe(top, padx = 10, pady = 10)
  entryDsname <- tcltk::ttkentry(dsnameFrame, width=30, textvariable=dsname)
  
  optionsFrame <- tcltk::tkframe(top, padx = 10, pady = 10)
  
  convert.factors <- tcltk::tclVar("1")
  convert.factorsCheckBox <- tcltk::ttkcheckbutton(optionsFrame, 
                                     text="Convert value labels to factor levels", variable=convert.factors)
  convert.dates <- tcltk::tclVar("1")
  convert.datesCheckBox <- tcltk::ttkcheckbutton(optionsFrame, text="Convert dates to R format", variable=convert.dates)

  missing.type <- tcltk::tclVar("1")
  missing.typeCheckBox <- tcltk::ttkcheckbutton(optionsFrame, text="Multiple missing types (>=Stata 8)"
                                          , variable=missing.type)
  convert.underscore <- tcltk::tclVar("1")
  convert.underscoreCheckBox <- tcltk::ttkcheckbutton(optionsFrame, text="Convert underscore to period"
                                                , variable=convert.underscore)
  warn.missing.labels <- tcltk::tclVar("1")
  warn.missing.labelsCheckBox <- tcltk::ttkcheckbutton(optionsFrame, text="Warn on missing labels", 
                                                variable=warn.missing.labels)
  
  tcltk::tkgrid(tcltk::tklabel(dsnameFrame, text="Enter name for data set:  "), entryDsname, sticky="w")
  tcltk::tkgrid(dsnameFrame, columnspan=2, sticky="w")
  tcltk::tkgrid(convert.datesCheckBox, sticky="w")
  tcltk::tkgrid(convert.factorsCheckBox, sticky="nw")
  tcltk::tkgrid(missing.typeCheckBox, sticky="w")
  tcltk::tkgrid(convert.underscoreCheckBox, sticky="w")
  tcltk::tkgrid(warn.missing.labelsCheckBox, sticky="w")
  tcltk::tkgrid(optionsFrame, sticky="w")
  
  buttonsFrame <- tcltk::tkframe(top, padx = 10, pady = 10)
  tfButOK <- tcltk::tkbutton(buttonsFrame, text = "OK", command = OnOK, width=10)
  tfButCanc <- tcltk::tkbutton(buttonsFrame, width=10, text = "Cancel", command = function() tcltk::tkdestroy(top))
  
  tcltk::tkgrid(tfButOK, tfButCanc)
  tcltk::tkgrid.configure(tfButCanc, padx=c(6,6))
  tcltk::tkgrid.columnconfigure(buttonsFrame, 0, weight=2)
  tcltk::tkgrid.columnconfigure(buttonsFrame, 1, weight=1)
  
  tcltk::tkgrid(buttonsFrame, sticky="ew")
  tcltk::tkwait.window(top)

  if(!is.null(lst)){
    lst <- list(dsname=env.dsname, convert.factors=as.numeric(env.convert.factors), 
                convert.dates=as.numeric(env.convert.dates), convert.underscore=as.numeric(env.convert.underscore),
                missing.type=as.numeric(env.missing.type), warn.missing.labels=as.numeric(env.warn.missing.labels)  )
  }
  return(lst)
  
}
.ImportSYSTAT <-
function(datasetname = "dataset") {

  e1 <- environment()
  env.dsname <- character()
  env.to.data.frame <- logical()
  lst <- NULL
  
  top <- .InitDlg(350, 140, main="Import SYSTAT Dataset")

  OnOK <- function() {
    assign("lst", list(), envir = e1)
    assign("env.dsname", tcltk::tclvalue(dsname), envir = e1)
    assign("env.to.data.frame", tcltk::tclvalue(to.data.frame ), envir = e1)
    tcltk::tkdestroy(top)
  }

  dsname <- tcltk::tclVar(datasetname)
  dsnameFrame <- tcltk::tkframe(top, padx = 10, pady = 10)
  entryDsname <- tcltk::ttkentry(dsnameFrame, width=30, textvariable=dsname)
  
  optionsFrame <- tcltk::tkframe(top, padx = 10, pady = 10)
  to.data.frame <- tcltk::tclVar("1")
  to.data.frameCheckBox <- tcltk::ttkcheckbutton(optionsFrame, 
                                            text="Convert dataset to data.frame", variable=to.data.frame)

  tcltk::tkgrid(tcltk::tklabel(dsnameFrame, text="Enter name for data set:  "), entryDsname, sticky="w")
  tcltk::tkgrid(dsnameFrame, columnspan=2, sticky="w")
  tcltk::tkgrid(to.data.frameCheckBox, sticky="w")
  tcltk::tkgrid(optionsFrame, sticky="w")
  
  buttonsFrame <- tcltk::tkframe(top, padx = 10, pady = 10)
  tfButOK <- tcltk::tkbutton(buttonsFrame, text = "OK", command = OnOK, width=10)
  tfButCanc <- tcltk::tkbutton(buttonsFrame, width=10, text = "Cancel", command = function() tcltk::tkdestroy(top))
  
  tcltk::tkgrid(tfButOK, tfButCanc)
  tcltk::tkgrid.configure(tfButCanc, padx=c(6,6))
  tcltk::tkgrid.columnconfigure(buttonsFrame, 0, weight=2)
  tcltk::tkgrid.columnconfigure(buttonsFrame, 1, weight=1)
  
  tcltk::tkgrid(buttonsFrame, sticky="ew")
  tcltk::tkwait.window(top)
  
  if(!is.null(lst)){
    lst <- list(dsname=env.dsname, to.data.frame=as.numeric(env.to.data.frame))
  }
  return(lst)
  
}
.InitDlg <-
function(width, height, x=NULL, y=NULL, resizex=FALSE, resizey=FALSE, main="Dialog", ico="R"){
  
  top <- tcltk::tktoplevel()
  
  if(is.null(x)) x <- as.integer(tcltk::tkwinfo("screenwidth", top))/2 - 50
  if(is.null(y)) y <- as.integer(tcltk::tkwinfo("screenheight", top))/2 - 25
  geom <- gettextf("%sx%s+%s+%s", width, height, x, y)
  tcltk::tkwm.geometry(top, geom)
  tcltk::tkwm.title(top, main)
  tcltk::tkwm.resizable(top, resizex, resizey)
  tcltk::tkwm.iconbitmap(top, file.path(find.package("DescTools"), "extdata", paste(ico, "ico", sep=".")))  
  
  return(top)
  
}
.nctCI <-
function(tval.1, df, conf) {       
  
  # Function for finding the upper and lower confidence limits for the noncentrality from noncentral t distributions.
  # Especially helpful when forming confidence intervals around the standardized effect size, Cohen's d.
  
  ###################################################################################################################
  # The following code was adapted from code written by Michael Smithson:
  # Australian National University, sometime around the early part of October, 2001
  # Adapted by Joe Rausch & Ken Kelley: University of Notre Dame, in January 2002.
  # Available at: JRausch@nd.edu & KKelley@nd.edu
  ###################################################################################################################
  
  
  # tval.1 is the observed t value, df is the degrees of freedom (group size need not be equal), and conf is simply 1 - alpha
  
  #         Result <- matrix(NA,1,4)
  tval <- abs(tval.1)
  
  
  ############################This part Finds the Lower bound for the confidence interval###########################
  ulim <- 1 - (1-conf)/2
  
  # This first part finds a lower value from which to start.
  lc <- c(-tval,tval/2,tval)
  while(pt(tval, df, lc[1])<ulim)    {
    lc <- c(lc[1]-tval,lc[1],lc[3])
  }
  
  # This next part finds the lower limit for the ncp.
  diff <- 1
  while(diff > .00000001)    {
    if(pt(tval, df, lc[2]) <ulim)
      lc <- c(lc[1],(lc[1]+lc[2])/2,lc[2])
    else lc <- c(lc[2],(lc[2]+lc[3])/2,lc[3])
    diff <- abs(pt(tval,df,lc[2]) - ulim)
    ucdf <- pt(tval,df,lc[2])
  }
  res.1 <- ifelse(tval.1 >= 0,lc[2],-lc[2])
  
  ############################This part Finds the Upper bound for the confidence interval###########################
  llim <- (1-conf)/2
  
  # This first part finds an upper value from which to start.
  uc <- c(tval,1.5*tval,2*tval)
  while(pt(tval,df,uc[3])>llim)   {
    uc <- c(uc[1],uc[3],uc[3]+tval)
  }
  
  # This next part finds the upper limit for the ncp.
  diff <- 1
  while(diff > .00000001)         {
    if(pt(tval,df,uc[2])<llim)
      uc <- c(uc[1],(uc[1]+uc[2])/2,uc[2])
    else uc <- c(uc[2],(uc[2]+uc[3])/2,uc[3])
    diff <- abs(pt(tval,df,uc[2]) - llim)
    lcdf <- pt(tval,df,uc[2])
  }
  res <- ifelse(tval.1 >= 0,uc[2],-uc[2])
  
  
  #################################This part Compiles the results into a matrix#####################################
  
  return(c(lwr.ci=min(res, res.1), lprob=ucdf, upr.ci=max(res, res.1), uprob=lcdf))
  
  #        Result[1,1] <- min(res,res.1)
  #         Result[1,2] <- ucdf
  #         Result[1,3] <- max(res,res.1)
  #         Result[1,4] <- lcdf
  # dimnames(Result) <- list("Values", c("Lower.Limit", "Prob.Low.Limit", "Upper.Limit", "Prob.Up.Limit"))
  #         Result
}
.pAD <-
function(q, n=Inf, lower.tail=TRUE, fast=TRUE) {
  q <- as.numeric(q)
  p <- rep(NA_real_, length(q))
  if(any(ones <- is.infinite(q) & (q == Inf)))
    p[ones] <- 1
  if(any(zeroes <- (is.finite(q) & q <= 0) | (is.infinite(q) & (q == -Inf))))
    p[zeroes] <- 0
  ok <- is.finite(q) & (q > 0)
  nok <- sum(ok)
  if(nok > 0) {
    if(is.finite(n)) {
      z <- .C("ADprobN",
              a       = as.double(q[ok]),
              na      = as.integer(nok),
              nsample = as.integer(n),
              prob    = as.double(numeric(nok))
      )
      p[ok] <- z$prob
    } else if(fast) {
      ## fast version adinf()
      z <- .C("ADprobApproxInf",
              a    = as.double(q[ok]),
              na   = as.integer(nok),
              prob = as.double(numeric(nok))
      )
      p[ok] <- z$prob
    } else {
      ## slow, accurate version ADinf()
      z <- .C("ADprobExactInf",
              a    = as.double(q[ok]),
              na   = as.integer(nok),
              prob = as.double(numeric(nok))
      )
      p[ok] <- z$prob
    }
    
  }
  if(!lower.tail)
    p <- 1 - p
  return(p)
}
.PageDF <-
structure(list(NA_real_, NA_real_, k3 = c(0.166666666666667, 
0.333333333333333, 0, 0.333333333333333, 0.166666666666667), 
    k4 = c(0.0416666666666667, 0.125, 0.0416666666666667, 0.166666666666667, 
    0.0833333333333333, 0.0833333333333333, 0.0833333333333333, 
    0.166666666666667, 0.0416666666666667, 0.125, 0.0416666666666667
    ), k5 = c(0.00833333333333333, 0.0333333333333333, 0.025, 
    0.05, 0.0583333333333333, 0.05, 0.0333333333333333, 0.0833333333333333, 
    0.05, 0.0833333333333333, 0.05, 0.0833333333333333, 0.05, 
    0.0833333333333333, 0.0333333333333333, 0.05, 0.0583333333333333, 
    0.05, 0.025, 0.0333333333333333, 0.00833333333333333), k6 = c(0.00138888888888889, 
    0.00694444444444444, 0.00833333333333333, 0.0125, 0.0222222222222222, 
    0.0166666666666667, 0.0194444444444444, 0.0333333333333333, 
    0.0277777777777778, 0.0291666666666667, 0.0319444444444444, 
    0.0388888888888889, 0.0333333333333333, 0.0472222222222222, 
    0.0277777777777778, 0.0444444444444444, 0.0583333333333333, 
    0.0402777777777778, 0.0402777777777778, 0.0583333333333333, 
    0.0444444444444444, 0.0277777777777778, 0.0472222222222222, 
    0.0333333333333333, 0.0388888888888889, 0.0319444444444444, 
    0.0291666666666667, 0.0277777777777778, 0.0333333333333333, 
    0.0194444444444444, 0.0166666666666667, 0.0222222222222222, 
    0.0125, 0.00833333333333333, 0.00694444444444444, 0.00138888888888889
    ), k7 = c(0.000198412698412698, 0.00119047619047619, 0.00198412698412698, 
    0.00277777777777778, 0.00575396825396825, 0.00515873015873016, 
    0.00694444444444444, 0.00912698412698413, 0.0109126984126984, 
    0.0107142857142857, 0.0146825396825397, 0.0138888888888889, 
    0.0166666666666667, 0.0178571428571429, 0.0154761904761905, 
    0.0178571428571429, 0.0255952380952381, 0.021031746031746, 
    0.0244047619047619, 0.0265873015873016, 0.0291666666666667, 
    0.0194444444444444, 0.0333333333333333, 0.0257936507936508, 
    0.0347222222222222, 0.0285714285714286, 0.0333333333333333, 
    0.0285714285714286, 0.0365079365079365, 0.0285714285714286, 
    0.0333333333333333, 0.0285714285714286, 0.0347222222222222, 
    0.0257936507936508, 0.0333333333333333, 0.0194444444444444, 
    0.0291666666666667, 0.0265873015873016, 0.0244047619047619, 
    0.021031746031746, 0.0255952380952381, 0.0178571428571429, 
    0.0154761904761905, 0.0178571428571429, 0.0166666666666667, 
    0.0138888888888889, 0.0146825396825397, 0.0107142857142857, 
    0.0109126984126984, 0.00912698412698413, 0.00694444444444444, 
    0.00515873015873016, 0.00575396825396825, 0.00277777777777778, 
    0.00198412698412698, 0.00119047619047619, 0.000198412698412698
    ), k8 = c(0.0000248015873015873, 0.000173611111111111, 0.00037202380952381, 
    0.000545634920634921, 0.0011656746031746, 0.00133928571428571, 
    0.00173611111111111, 0.00233134920634921, 0.00319940476190476, 
    0.00307539682539683, 0.00441468253968254, 0.00453869047619048, 
    0.00587797619047619, 0.00590277777777778, 0.0068452380952381, 
    0.00654761904761905, 0.00939980158730159, 0.00865575396825397, 
    0.00942460317460317, 0.00992063492063492, 0.0128224206349206, 
    0.0097718253968254, 0.0134424603174603, 0.012202380952381, 
    0.0158730158730159, 0.0138144841269841, 0.0165178571428571, 
    0.0147569444444444, 0.0192460317460317, 0.0169642857142857, 
    0.0194940476190476, 0.0178075396825397, 0.0228670634920635, 
    0.0184771825396825, 0.0227430555555556, 0.0193700396825397, 
    0.0243551587301587, 0.0204861111111111, 0.0235615079365079, 
    0.0209325396825397, 0.0264384920634921, 0.0209573412698413, 
    0.0232142857142857, 0.0209573412698413, 0.0264384920634921, 
    0.0209325396825397, 0.0235615079365079, 0.0204861111111111, 
    0.0243551587301587, 0.0193700396825397, 0.0227430555555556, 
    0.0184771825396825, 0.0228670634920635, 0.0178075396825397, 
    0.0194940476190476, 0.0169642857142857, 0.0192460317460317, 
    0.0147569444444444, 0.0165178571428571, 0.0138144841269841, 
    0.0158730158730159, 0.012202380952381, 0.0134424603174603, 
    0.0097718253968254, 0.0128224206349206, 0.00992063492063492, 
    0.00942460317460317, 0.00865575396825397, 0.00939980158730159, 
    0.00654761904761905, 0.0068452380952381, 0.00590277777777778, 
    0.00587797619047619, 0.00453869047619048, 0.00441468253968254, 
    0.00307539682539683, 0.00319940476190476, 0.00233134920634921, 
    0.00173611111111111, 0.00133928571428571, 0.0011656746031746, 
    0.000545634920634921, 0.00037202380952381, 0.000173611111111111, 
    0.0000248015873015873), k9 = c(0.00000275573192239859, 0.0000220458553791887, 
    0.0000578703703703704, 0.000093694885361552, 0.000198412698412698, 
    0.000281084656084656, 0.000358245149911817, 0.000523589065255732, 
    0.000716490299823633, 0.000782627865961199, 0.00109678130511464, 
    0.00125110229276896, 0.00152943121693122, 0.00169753086419753, 
    0.00208333333333333, 0.00205026455026455, 0.00281635802469136, 
    0.00287147266313933, 0.00319389329805996, 0.00353284832451499, 
    0.00428516313932981, 0.00383597883597884, 0.00473710317460317, 
    0.00484457671957672, 0.00553626543209877, 0.00559964726631393, 
    0.00628858024691358, 0.00610119047619048, 0.00737433862433862, 
    0.00713734567901235, 0.00793099647266314, 0.00806878306878307, 
    0.00936122134038801, 0.00864748677248677, 0.0100501543209877, 
    0.00983245149911817, 0.0108052248677249, 0.0106536596119929, 
    0.0118799603174603, 0.0111607142857143, 0.013370811287478, 
    0.0123787477954145, 0.0132716049382716, 0.0131834215167548, 
    0.0151703042328042, 0.0136518959435626, 0.0155368165784832, 
    0.0146164021164021, 0.0162312610229277, 0.0151179453262787, 
    0.0170524691358025, 0.015162037037037, 0.0177358906525573, 
    0.0160438712522046, 0.0171764770723104, 0.0166005291005291, 
    0.0184551366843034, 0.0157627865961199, 0.0183862433862434, 
    0.0165895061728395, 0.0184303350970018, 0.0165895061728395, 
    0.0183862433862434, 0.0157627865961199, 0.0184551366843034, 
    0.0166005291005291, 0.0171764770723104, 0.0160438712522046, 
    0.0177358906525573, 0.015162037037037, 0.0170524691358025, 
    0.0151179453262787, 0.0162312610229277, 0.0146164021164021, 
    0.0155368165784832, 0.0136518959435626, 0.0151703042328042, 
    0.0131834215167548, 0.0132716049382716, 0.0123787477954145, 
    0.013370811287478, 0.0111607142857143, 0.0118799603174603, 
    0.0106536596119929, 0.0108052248677249, 0.00983245149911817, 
    0.0100501543209877, 0.00864748677248677, 0.00936122134038801, 
    0.00806878306878307, 0.00793099647266314, 0.00713734567901235, 
    0.00737433862433862, 0.00610119047619048, 0.00628858024691358, 
    0.00559964726631393, 0.00553626543209877, 0.00484457671957672, 
    0.00473710317460317, 0.00383597883597884, 0.00428516313932981, 
    0.00353284832451499, 0.00319389329805996, 0.00287147266313933, 
    0.00281635802469136, 0.00205026455026455, 0.00208333333333333, 
    0.00169753086419753, 0.00152943121693122, 0.00125110229276896, 
    0.00109678130511464, 0.000782627865961199, 0.000716490299823633, 
    0.000523589065255732, 0.000358245149911817, 0.000281084656084656, 
    0.000198412698412698, 0.000093694885361552, 0.0000578703703703704, 
    0.0000220458553791887, 0.00000275573192239859), k10 = c(0.000000275573192239859, 
    0.00000248015873015873, 0.00000771604938271605, 0.0000140542328042328, 
    0.0000294863315696649, 0.000048776455026455, 0.000064484126984127, 
    0.0000992063492063492, 0.00013723544973545, 0.000170579805996473, 
    0.000225694444444444, 0.000286596119929453, 0.000345017636684303, 
    0.000421075837742504, 0.000502645502645503, 0.000553902116402116, 
    0.000698026895943563, 0.00078180114638448, 0.000876322751322751, 
    0.00101300705467372, 0.00118634259259259, 0.00123815035273369, 
    0.00141369047619048, 0.00156305114638448, 0.00169642857142857, 
    0.00190393518518519, 0.00204585537918871, 0.0021577380952381, 
    0.00241760361552028, 0.00258818342151675, 0.00272597001763668, 
    0.00294257054673721, 0.00320960097001764, 0.00334573412698413, 
    0.0035896164021164, 0.00383542768959436, 0.00400104717813051, 
    0.00430197310405644, 0.00448578042328042, 0.00462411816578483, 
    0.005149360670194, 0.00531305114638448, 0.00539737654320988, 
    0.00573054453262787, 0.00616870590828924, 0.00630952380952381, 
    0.00659942680776014, 0.00688106261022928, 0.00716820987654321, 
    0.00746693121693122, 0.00784474206349206, 0.00783371913580247, 
    0.00841600529100529, 0.00861772486772487, 0.00875606261022928, 
    0.00916666666666667, 0.00957561728395062, 0.00950699955908289, 
    0.0100030313051146, 0.0100848765432099, 0.010597442680776, 
    0.0107269620811287, 0.0110185185185185, 0.0108537257495591, 
    0.0115928130511464, 0.0118077601410935, 0.0116909171075838, 
    0.0118289792768959, 0.0124129188712522, 0.0122861552028219, 
    0.0126873897707231, 0.0125374779541446, 0.012921626984127, 
    0.012961860670194, 0.0130459104938272, 0.0131299603174603, 
    0.01347194664903, 0.0134589947089947, 0.0132396384479718, 
    0.0133763227513228, 0.0137968474426808, 0.0134948192239859, 
    0.013520171957672, 0.013520171957672, 0.0134948192239859, 
    0.0137968474426808, 0.0133763227513228, 0.0132396384479718, 
    0.0134589947089947, 0.01347194664903, 0.0131299603174603, 
    0.0130459104938272, 0.012961860670194, 0.012921626984127, 
    0.0125374779541446, 0.0126873897707231, 0.0122861552028219, 
    0.0124129188712522, 0.0118289792768959, 0.0116909171075838, 
    0.0118077601410935, 0.0115928130511464, 0.0108537257495591, 
    0.0110185185185185, 0.0107269620811287, 0.010597442680776, 
    0.0100848765432099, 0.0100030313051146, 0.00950699955908289, 
    0.00957561728395062, 0.00916666666666667, 0.00875606261022928, 
    0.00861772486772487, 0.00841600529100529, 0.00783371913580247, 
    0.00784474206349206, 0.00746693121693122, 0.00716820987654321, 
    0.00688106261022928, 0.00659942680776014, 0.00630952380952381, 
    0.00616870590828924, 0.00573054453262787, 0.00539737654320988, 
    0.00531305114638448, 0.005149360670194, 0.00462411816578483, 
    0.00448578042328042, 0.00430197310405644, 0.00400104717813051, 
    0.00383542768959436, 0.0035896164021164, 0.00334573412698413, 
    0.00320960097001764, 0.00294257054673721, 0.00272597001763668, 
    0.00258818342151675, 0.00241760361552028, 0.0021577380952381, 
    0.00204585537918871, 0.00190393518518519, 0.00169642857142857, 
    0.00156305114638448, 0.00141369047619048, 0.00123815035273369, 
    0.00118634259259259, 0.00101300705467372, 0.000876322751322751, 
    0.00078180114638448, 0.000698026895943563, 0.000553902116402116, 
    0.000502645502645503, 0.000421075837742504, 0.000345017636684303, 
    0.000286596119929453, 0.000225694444444444, 0.000170579805996473, 
    0.00013723544973545, 0.0000992063492063492, 0.000064484126984127, 
    0.000048776455026455, 0.0000294863315696649, 0.0000140542328042328, 
    0.00000771604938271605, 0.00000248015873015873, 0.000000275573192239859
    ), k11 = c(0.0000000250521083854417, 0.000000250521083854417, 
    0.000000901875901875902, 0.00000185385602052269, 0.00000390812890812891, 
    0.00000721500721500722, 0.0000102463123296457, 0.0000161335578002245, 
    0.0000232984607984608, 0.0000312149270482604, 0.0000409852493185827, 
    0.0000560666185666186, 0.0000679914221580888, 0.0000870811287477954, 
    0.000105168751002084, 0.000126563251563252, 0.000150989057239057, 
    0.000183030703864037, 0.00020560265351932, 0.000245911495911496, 
    0.000285193201859869, 0.000320767195767196, 0.000355564574314574, 
    0.00041967291967292, 0.000452917067500401, 0.000522787397787398, 
    0.000565676607343274, 0.000635972823472824, 0.000684749278499278, 
    0.000779421196087863, 0.000812890812890813, 0.000916706750040083, 
    0.000980539522206189, 0.00107989618406285, 0.00112178330928331, 
    0.0012658329324996, 0.00130180776014109, 0.00143904320987654, 
    0.0014867925284592, 0.00163159371492705, 0.00170028659611993, 
    0.00186883718133718, 0.00188552188552189, 0.00207792207792208, 
    0.0021575627304794, 0.00233129910213244, 0.00236040965207632, 
    0.00260196208112875, 0.00263831269039602, 0.0028608505691839, 
    0.00291138067179734, 0.00313983084816418, 0.00319722522847523, 
    0.00346896544813211, 0.00346513247554914, 0.00377620450537117, 
    0.0038167638688472, 0.00408199054032387, 0.00409178591470258, 
    0.00443171797338464, 0.00445977633477633, 0.00477237654320988, 
    0.00476370851370851, 0.00510446729196729, 0.00513235028860029, 
    0.00551777697611031, 0.0054374348645182, 0.00583503687670354, 
    0.0058480138688472, 0.00621612954946288, 0.00614999198332532, 
    0.00658584856501523, 0.00653767336059003, 0.00694449454866122, 
    0.00683531746031746, 0.00733019680936348, 0.00722475248516915, 
    0.00768438351771685, 0.00750075156325156, 0.00800625300625301, 
    0.00788956028539362, 0.00834821428571429, 0.00812582671957672, 
    0.00866341991341991, 0.00847500300625301, 0.00902101370851371, 
    0.00869082691999359, 0.00926827801827802, 0.00904919733044733, 
    0.00956143779060446, 0.00925542628667629, 0.0098051948051948, 
    0.00950294111752445, 0.0100741041366041, 0.00965267756934424, 
    0.0102985710277377, 0.00993446368446368, 0.0104740359948693, 
    0.00999376202501203, 0.01066829004329, 0.010269535634119, 
    0.0107681978515312, 0.010281560646144, 0.0109201639409973, 
    0.0104340778819945, 0.0110250821709155, 0.0104440987253487, 
    0.0110324474907808, 0.0105516474266474, 0.0110792448292448, 
    0.0105339105339105, 0.0110792448292448, 0.0105516474266474, 
    0.0110324474907808, 0.0104440987253487, 0.0110250821709155, 
    0.0104340778819945, 0.0109201639409973, 0.010281560646144, 
    0.0107681978515312, 0.010269535634119, 0.01066829004329, 
    0.00999376202501203, 0.0104740359948693, 0.00993446368446368, 
    0.0102985710277377, 0.00965267756934424, 0.0100741041366041, 
    0.00950294111752445, 0.0098051948051948, 0.00925542628667629, 
    0.00956143779060446, 0.00904919733044733, 0.00926827801827802, 
    0.00869082691999359, 0.00902101370851371, 0.00847500300625301, 
    0.00866341991341991, 0.00812582671957672, 0.00834821428571429, 
    0.00788956028539362, 0.00800625300625301, 0.00750075156325156, 
    0.00768438351771685, 0.00722475248516915, 0.00733019680936348, 
    0.00683531746031746, 0.00694449454866122, 0.00653767336059003, 
    0.00658584856501523, 0.00614999198332532, 0.00621612954946288, 
    0.0058480138688472, 0.00583503687670354, 0.0054374348645182, 
    0.00551777697611031, 0.00513235028860029, 0.00510446729196729, 
    0.00476370851370851, 0.00477237654320988, 0.00445977633477633, 
    0.00443171797338464, 0.00409178591470258, 0.00408199054032387, 
    0.0038167638688472, 0.00377620450537117, 0.00346513247554914, 
    0.00346896544813211, 0.00319722522847523, 0.00313983084816418, 
    0.00291138067179734, 0.0028608505691839, 0.00263831269039602, 
    0.00260196208112875, 0.00236040965207632, 0.00233129910213244, 
    0.0021575627304794, 0.00207792207792208, 0.00188552188552189, 
    0.00186883718133718, 0.00170028659611993, 0.00163159371492705, 
    0.0014867925284592, 0.00143904320987654, 0.00130180776014109, 
    0.0012658329324996, 0.00112178330928331, 0.00107989618406285, 
    0.000980539522206189, 0.000916706750040083, 0.000812890812890813, 
    0.000779421196087863, 0.000684749278499278, 0.000635972823472824, 
    0.000565676607343274, 0.000522787397787398, 0.000452917067500401, 
    0.00041967291967292, 0.000355564574314574, 0.000320767195767196, 
    0.000285193201859869, 0.000245911495911496, 0.00020560265351932, 
    0.000183030703864037, 0.000150989057239057, 0.000126563251563252, 
    0.000105168751002084, 0.0000870811287477954, 0.0000679914221580888, 
    0.0000560666185666186, 0.0000409852493185827, 0.0000312149270482604, 
    0.0000232984607984608, 0.0000161335578002245, 0.0000102463123296457, 
    0.00000721500721500722, 0.00000390812890812891, 0.00000185385602052269, 
    0.000000901875901875902, 0.000000250521083854417, 0.0000000250521083854417
    ), k12 = c(0.00000000208767569878681, 0.0000000229644326866549, 
    0.0000000939454064454064, 0.000000217118272673828, 0.000000467639356528245, 
    0.000000933191037357704, 0.00000144258390786169, 0.00000230896932285821, 
    0.00000351773355245577, 0.00000493526535193202, 0.00000668891293891294, 
    0.00000942585578002245, 0.0000118914007802897, 0.0000156346033082144, 
    0.0000195176801079579, 0.0000243715261076372, 0.0000292065830260275, 
    0.000036820336299503, 0.0000422211533322644, 0.0000516783242477687, 
    0.0000605780857516969, 0.0000708598885682219, 0.0000795947236919459, 
    0.000095790911763134, 0.000105083156298434, 0.000122690613142002, 
    0.00013644422064561, 0.000156446241515686, 0.000169888785340174, 
    0.000197272827481161, 0.000209942931297098, 0.000240546169365614, 
    0.000259122307733419, 0.000291825747554914, 0.000308825690770135, 
    0.000352556233632623, 0.000367936140505585, 0.000412196952995564, 
    0.000434395208700764, 0.000482031792795682, 0.000503146544813211, 
    0.000564325881166159, 0.000578680739271017, 0.000642521862139918, 
    0.000668745156592379, 0.000734623850943295, 0.000753730258938592, 
    0.000836038961038961, 0.000853103622200844, 0.000937401879242157, 
    0.000962447724600502, 0.00105036601130351, 0.0010705183448239, 
    0.00117706287411149, 0.00118903151889263, 0.00130116058067447, 
    0.00132466363369141, 0.00143724363342419, 0.0014520264650473, 
    0.00158851870223398, 0.00159722222222222, 0.00173783761891401, 
    0.0017504116896478, 0.00189643625407514, 0.0019056387285554, 
    0.00207490538653733, 0.00207270915170221, 0.00224119919432419, 
    0.0022522033329325, 0.0024282486739084, 0.00242618813799369, 
    0.002630099774197, 0.00261513740246379, 0.00282247073913741, 
    0.00281329331676554, 0.00303191889129389, 0.00301681246993747, 
    0.00324910605726578, 0.00321702683247822, 0.00345998426727593, 
    0.00343727244334883, 0.00368747620049703, 0.00364376653439153, 
    0.0039182917134306, 0.0038663294652878, 0.0041509422933034, 
    0.0040934748443429, 0.00437854487333654, 0.00431584362139918, 
    0.00462541043704238, 0.00454670506319812, 0.00486188355111966, 
    0.00477621577881995, 0.00509947565937149, 0.00500108559136337, 
    0.00534815750093528, 0.00523622885602052, 0.0055775032901769, 
    0.00545801934690824, 0.00581804528419112, 0.00568706033549784, 
    0.00606205073218962, 0.00590264416653306, 0.00628478485249319, 
    0.00612468100315322, 0.00651244797512159, 0.00634126900619956, 
    0.00672875622962429, 0.0065451263628347, 0.00693813340080701, 
    0.00675369351584629, 0.00716084455667789, 0.00693856137432526, 
    0.00735628440489552, 0.00712298664555609, 0.00755223155830795, 
    0.00731027412016995, 0.00773119546991075, 0.00747214205547539, 
    0.00790157277136444, 0.00764436694992251, 0.00807329036061675, 
    0.00778211179252846, 0.00820795796924269, 0.00792523448773449, 
    0.00836143136056331, 0.00805332591790925, 0.00847926604002993, 
    0.00815746753246753, 0.00858683353040992, 0.00827313311688312, 
    0.00870108575837743, 0.00834683433207739, 0.00876951976778366, 
    0.00841913262920207, 0.00885124809603976, 0.00848748939460745, 
    0.008891799108813, 0.00852856650165678, 0.0089322311240714, 
    0.00856066451552563, 0.00896541472930362, 0.00856743902316819, 
    0.0089742163700497, 0.00856743902316819, 0.00896541472930362, 
    0.00856066451552563, 0.0089322311240714, 0.00852856650165678, 
    0.008891799108813, 0.00848748939460745, 0.00885124809603976, 
    0.00841913262920207, 0.00876951976778366, 0.00834683433207739, 
    0.00870108575837743, 0.00827313311688312, 0.00858683353040992, 
    0.00815746753246753, 0.00847926604002993, 0.00805332591790925, 
    0.00836143136056331, 0.00792523448773449, 0.00820795796924269, 
    0.00778211179252846, 0.00807329036061675, 0.00764436694992251, 
    0.00790157277136444, 0.00747214205547539, 0.00773119546991075, 
    0.00731027412016995, 0.00755223155830795, 0.00712298664555609, 
    0.00735628440489552, 0.00693856137432526, 0.00716084455667789, 
    0.00675369351584629, 0.00693813340080701, 0.0065451263628347, 
    0.00672875622962429, 0.00634126900619956, 0.00651244797512159, 
    0.00612468100315322, 0.00628478485249319, 0.00590264416653306, 
    0.00606205073218962, 0.00568706033549784, 0.00581804528419112, 
    0.00545801934690824, 0.0055775032901769, 0.00523622885602052, 
    0.00534815750093528, 0.00500108559136337, 0.00509947565937149, 
    0.00477621577881995, 0.00486188355111966, 0.00454670506319812, 
    0.00462541043704238, 0.00431584362139918, 0.00437854487333654, 
    0.0040934748443429, 0.0041509422933034, 0.0038663294652878, 
    0.0039182917134306, 0.00364376653439153, 0.00368747620049703, 
    0.00343727244334883, 0.00345998426727593, 0.00321702683247822, 
    0.00324910605726578, 0.00301681246993747, 0.00303191889129389, 
    0.00281329331676554, 0.00282247073913741, 0.00261513740246379, 
    0.002630099774197, 0.00242618813799369, 0.0024282486739084, 
    0.0022522033329325, 0.00224119919432419, 0.00207270915170221, 
    0.00207490538653733, 0.0019056387285554, 0.00189643625407514, 
    0.0017504116896478, 0.00173783761891401, 0.00159722222222222, 
    0.00158851870223398, 0.0014520264650473, 0.00143724363342419, 
    0.00132466363369141, 0.00130116058067447, 0.00118903151889263, 
    0.00117706287411149, 0.0010705183448239, 0.00105036601130351, 
    0.000962447724600502, 0.000937401879242157, 0.000853103622200844, 
    0.000836038961038961, 0.000753730258938592, 0.000734623850943295, 
    0.000668745156592379, 0.000642521862139918, 0.000578680739271017, 
    0.000564325881166159, 0.000503146544813211, 0.000482031792795682, 
    0.000434395208700764, 0.000412196952995564, 0.000367936140505585, 
    0.000352556233632623, 0.000308825690770135, 0.000291825747554914, 
    0.000259122307733419, 0.000240546169365614, 0.000209942931297098, 
    0.000197272827481161, 0.000169888785340174, 0.000156446241515686, 
    0.00013644422064561, 0.000122690613142002, 0.000105083156298434, 
    0.000095790911763134, 0.0000795947236919459, 0.0000708598885682219, 
    0.0000605780857516969, 0.0000516783242477687, 0.0000422211533322644, 
    0.000036820336299503, 0.0000292065830260275, 0.0000243715261076372, 
    0.0000195176801079579, 0.0000156346033082144, 0.0000118914007802897, 
    0.00000942585578002245, 0.00000668891293891294, 0.00000493526535193202, 
    0.00000351773355245577, 0.00000230896932285821, 0.00000144258390786169, 
    0.000000933191037357704, 0.000000467639356528245, 0.000000217118272673828, 
    0.0000000939454064454064, 0.0000000229644326866549, 0.00000000208767569878681
    ), k13 = c(0.000000000160590438368216, 0.00000000192708526041859, 
    0.00000000883247411025189, 0.0000000228038422482867, 0.0000000509071689627245, 
    0.000000107595593706705, 0.00000018098542404098, 0.000000296128768350991, 
    0.000000473902383624606, 0.000000692144789367012, 0.0000009776745887857, 
    0.00000139842153731043, 0.00000186076140937252, 0.00000247598337876116, 
    0.00000321437821437821, 0.00000410629750907529, 0.0000050854174118063, 
    0.00000646248042081375, 0.00000771235580263358, 0.00000952044354822133, 
    0.0000114183013488569, 0.0000136543626126959, 0.000015789251900363, 
    0.0000190367117450451, 0.0000216729643812977, 0.0000253797128797129, 
    0.0000289560619421731, 0.0000334522730356064, 0.0000374554714832493, 
    0.0000433459287625954, 0.0000477030685364019, 0.0000545962525129192, 
    0.0000603718876288321, 0.0000681879848546515, 0.0000742293971460638, 
    0.0000843006658978881, 0.0000910514061555728, 0.000101773547954104, 
    0.0001101725884712, 0.000122259106634107, 0.000131219571323738, 
    0.000146096187762854, 0.000154937654937655, 0.000171188122577011, 
    0.00018283558648142, 0.000200052326788438, 0.00021120436919048, 
    0.00023237821848933, 0.000244531381684159, 0.00026650818317485, 
    0.000280558882989439, 0.000304558481641815, 0.000318997970907693, 
    0.000347230572925017, 0.000360625260798872, 0.000390587100656545, 
    0.000408514453653343, 0.00043919332981833, 0.00045533812894924, 
    0.000492720371192593, 0.000509165635033691, 0.000547387283498395, 
    0.000566219563615397, 0.000607545104072882, 0.000626266416196972, 
    0.000673345751470752, 0.00069041972687806, 0.000738891702433369, 
    0.000762127532960866, 0.000811870100064545, 0.000830996421274199, 
    0.000890629753476976, 0.000910561917506362, 0.000969726325629103, 
    0.000992177511274733, 0.00105533548241882, 0.00107853212887935, 
    0.00114782947248225, 0.00116776179710207, 0.00124006073658851, 
    0.0012662933452864, 0.00134045513385791, 0.00136029736724181, 
    0.00144613616835839, 0.00146752520884465, 0.00155180178617679, 
    0.00157579255235505, 0.00166455747184914, 0.00168833657982964, 
    0.00178474528300917, 0.00180269062213507, 0.00190424030701808, 
    0.00192908750200417, 0.00203083278604112, 0.002048244001369, 
    0.00216215690174024, 0.00218261644476922, 0.00229300277911389, 
    0.00231340434899463, 0.00243171437615882, 0.00245050153036264, 
    0.00257718972128694, 0.00258640343709788, 0.00271782583414528, 
    0.0027371223812196, 0.00286510204044926, 0.00287515275362498, 
    0.0030170218798691, 0.00302966773452885, 0.00316704482503094, 
    0.00317577387889888, 0.00332468778649334, 0.00332984675432592, 
    0.00348349374391041, 0.00347915539321789, 0.00363754365490477, 
    0.00364376460730627, 0.00379923991903159, 0.00379090945063167, 
    0.00396008730210119, 0.00395407062073729, 0.00412015646390646, 
    0.00410566237389154, 0.00428067560012004, 0.00426714280446919, 
    0.00444324451268896, 0.00441917907195685, 0.00459934580594303, 
    0.00458380852044047, 0.0047604282934144, 0.00472836047054797, 
    0.00492090471257138, 0.00488893645577673, 0.00507508116883117, 
    0.00503182854311327, 0.0052295386583581, 0.00519026963905436, 
    0.00537987893022615, 0.00532753656451573, 0.00552678802678803, 
    0.00547766068165374, 0.00567561393082226, 0.00560605739425184, 
    0.00581947437850216, 0.00575282051410524, 0.00595092439710495, 
    0.00587508379609074, 0.00608207860812027, 0.00601133129344935, 
    0.00621179746179746, 0.00612085108178858, 0.0063354366826589, 
    0.00624494172237228, 0.00645268954296732, 0.00634411049341605, 
    0.00656078071876683, 0.00646593584527612, 0.00666376768807324, 
    0.00654714707232068, 0.00676277972284917, 0.00664923312926785, 
    0.00685281025558803, 0.00672129632199077, 0.00693663204079871, 
    0.0068084701114215, 0.00701254571046238, 0.00686978289842873, 
    0.00707617067860123, 0.0069428483360775, 0.0071408738509433, 
    0.00697955015663349, 0.00719267518746685, 0.00703935162702524, 
    0.00723436767707601, 0.00706581404064043, 0.00726824294532628, 
    0.00710744068816985, 0.00729284090395201, 0.00711844482677816, 
    0.00730942186671353, 0.00714179580064997, 0.00732270141124308, 
    0.00713104121958289, 0.00732270141124308, 0.00714179580064997, 
    0.00730942186671353, 0.00711844482677816, 0.00729284090395201, 
    0.00710744068816985, 0.00726824294532628, 0.00706581404064043, 
    0.00723436767707601, 0.00703935162702524, 0.00719267518746685, 
    0.00697955015663349, 0.0071408738509433, 0.0069428483360775, 
    0.00707617067860123, 0.00686978289842873, 0.00701254571046238, 
    0.0068084701114215, 0.00693663204079871, 0.00672129632199077, 
    0.00685281025558803, 0.00664923312926785, 0.00676277972284917, 
    0.00654714707232068, 0.00666376768807324, 0.00646593584527612, 
    0.00656078071876683, 0.00634411049341605, 0.00645268954296732, 
    0.00624494172237228, 0.0063354366826589, 0.00612085108178858, 
    0.00621179746179746, 0.00601133129344935, 0.00608207860812027, 
    0.00587508379609074, 0.00595092439710495, 0.00575282051410524, 
    0.00581947437850216, 0.00560605739425184, 0.00567561393082226, 
    0.00547766068165374, 0.00552678802678803, 0.00532753656451573, 
    0.00537987893022615, 0.00519026963905436, 0.0052295386583581, 
    0.00503182854311327, 0.00507508116883117, 0.00488893645577673, 
    0.00492090471257138, 0.00472836047054797, 0.0047604282934144, 
    0.00458380852044047, 0.00459934580594303, 0.00441917907195685, 
    0.00444324451268896, 0.00426714280446919, 0.00428067560012004, 
    0.00410566237389154, 0.00412015646390646, 0.00395407062073729, 
    0.00396008730210119, 0.00379090945063167, 0.00379923991903159, 
    0.00364376460730627, 0.00363754365490477, 0.00347915539321789, 
    0.00348349374391041, 0.00332984675432592, 0.00332468778649334, 
    0.00317577387889888, 0.00316704482503094, 0.00302966773452885, 
    0.0030170218798691, 0.00287515275362498, 0.00286510204044926, 
    0.0027371223812196, 0.00271782583414528, 0.00258640343709788, 
    0.00257718972128694, 0.00245050153036264, 0.00243171437615882, 
    0.00231340434899463, 0.00229300277911389, 0.00218261644476922, 
    0.00216215690174024, 0.002048244001369, 0.00203083278604112, 
    0.00192908750200417, 0.00190424030701808, 0.00180269062213507, 
    0.00178474528300917, 0.00168833657982964, 0.00166455747184914, 
    0.00157579255235505, 0.00155180178617679, 0.00146752520884465, 
    0.00144613616835839, 0.00136029736724181, 0.00134045513385791, 
    0.0012662933452864, 0.00124006073658851, 0.00116776179710207, 
    0.00114782947248225, 0.00107853212887935, 0.00105533548241882, 
    0.000992177511274733, 0.000969726325629103, 0.000910561917506362, 
    0.000890629753476976, 0.000830996421274199, 0.000811870100064545, 
    0.000762127532960866, 0.000738891702433369, 0.00069041972687806, 
    0.000673345751470752, 0.000626266416196972, 0.000607545104072882, 
    0.000566219563615397, 0.000547387283498395, 0.000509165635033691, 
    0.000492720371192593, 0.00045533812894924, 0.00043919332981833, 
    0.000408514453653343, 0.000390587100656545, 0.000360625260798872, 
    0.000347230572925017, 0.000318997970907693, 0.000304558481641815, 
    0.000280558882989439, 0.00026650818317485, 0.000244531381684159, 
    0.00023237821848933, 0.00021120436919048, 0.000200052326788438, 
    0.00018283558648142, 0.000171188122577011, 0.000154937654937655, 
    0.000146096187762854, 0.000131219571323738, 0.000122259106634107, 
    0.0001101725884712, 0.000101773547954104, 0.0000910514061555728, 
    0.0000843006658978881, 0.0000742293971460638, 0.0000681879848546515, 
    0.0000603718876288321, 0.0000545962525129192, 0.0000477030685364019, 
    0.0000433459287625954, 0.0000374554714832493, 0.0000334522730356064, 
    0.0000289560619421731, 0.0000253797128797129, 0.0000216729643812977, 
    0.0000190367117450451, 0.000015789251900363, 0.0000136543626126959, 
    0.0000114183013488569, 0.00000952044354822133, 0.00000771235580263358, 
    0.00000646248042081375, 0.0000050854174118063, 0.00000410629750907529, 
    0.00000321437821437821, 0.00000247598337876116, 0.00000186076140937252, 
    0.00000139842153731043, 0.0000009776745887857, 0.000000692144789367012, 
    0.000000473902383624606, 0.000000296128768350991, 0.00000018098542404098, 
    0.000000107595593706705, 0.0000000509071689627245, 0.0000000228038422482867, 
    0.00000000883247411025189, 0.00000000192708526041859, 0.000000000160590438368216
    ), k14 = c(0.0000000000114707455977297, 0.000000000149119692770486, 
    0.000000000757069209450162, 0.00000000216797091797092, 0.00000000507006955419654, 
    0.0000000112183891945797, 0.0000000203949856727634, 0.0000000344237075387869, 
    0.0000000573996109710395, 0.0000000874185522002982, 0.000000128300289510607, 
    0.000000186927270260604, 0.000000260546515506833, 0.000000352002770157532, 
    0.000000474074444808572, 0.000000618158480261655, 0.000000791481446243351, 
    0.00000101300448522671, 0.00000126104788803202, 0.0000015632561515498, 
    0.00000192783085888245, 0.00000233994033597208, 0.00000278841207660652, 
    0.00000336826973731736, 0.0000039708280035661, 0.00000466222719446926, 
    0.0000054643879048641, 0.00000636729617384379, 0.00000732332546568658, 
    0.00000846519230695818, 0.00000963658484739834, 0.0000109976232248058, 
    0.0000124950487673702, 0.0000141262232036042, 0.0000158065268432332, 
    0.0000178771455433162, 0.0000199177453021699, 0.0000221578327977137, 
    0.0000246266928434587, 0.0000273414742040734, 0.0000300917345808219, 
    0.0000332843527908012, 0.0000364693200134668, 0.0000400215116856982, 
    0.0000438300859927844, 0.0000478252663892545, 0.0000519010058320574, 
    0.0000566944124731823, 0.0000614033026607431, 0.0000664096866353811, 
    0.0000717884454243581, 0.0000776080593788927, 0.0000833807809254238, 
    0.0000898906011133194, 0.0000962794049351589, 0.000103324794235012, 
    0.000110736100319434, 0.000118326281210706, 0.000125959362690513, 
    0.000134851358499672, 0.000143310092776859, 0.000152377694230373, 
    0.000161791436903044, 0.000172089241409678, 0.000181894744456748, 
    0.000193042898276033, 0.000203714327908276, 0.000215648537511137, 
    0.000227581278858561, 0.000240158228749499, 0.000252292189916198, 
    0.00026678346959845, 0.000280071215711097, 0.000294500496013393, 
    0.000308943770625318, 0.000325053056327858, 0.000340230848663388, 
    0.000357225262979231, 0.000373243195663831, 0.000391510702150583, 
    0.000409016551129646, 0.000427895849832854, 0.000445824728438816, 
    0.000467014843254923, 0.000486249620364204, 0.000507307844547428, 
    0.000527954945737684, 0.000551197108116751, 0.00057271638744853, 
    0.000596600682166158, 0.000618958220644729, 0.000645143879580884, 
    0.000669183419369431, 0.000695245113957912, 0.000720100717000519, 
    0.000749029914456502, 0.000775465325936556, 0.000804134286587164, 
    0.000831468660399712, 0.000863092267172128, 0.000891933506950868, 
    0.000923561690550778, 0.000953170667332374, 0.000988031685576328, 
    0.00101921268215911, 0.00105285567928177, 0.00108576340161162, 
    0.00112304591719274, 0.00115673808940178, 0.00119389480531594, 
    0.00122807648012261, 0.00126878552535795, 0.00130536346186148, 
    0.0013445000284658, 0.00138128344043523, 0.0014252162125426, 
    0.00146345127030891, 0.00150539796311126, 0.00154541778859735, 
    0.00159092303933574, 0.00163147104677363, 0.00167717257343993, 
    0.00171767989414319, 0.00176663042920483, 0.00181047798514316, 
    0.0018562583846562, 0.00189980407645338, 0.00195280787976721, 
    0.00199630062260271, 0.00204512066646243, 0.00209187114693067, 
    0.00214484907224243, 0.00219115484337459, 0.00224380815805667, 
    0.00228872475307247, 0.00234531358880317, 0.00239541632584787, 
    0.00244614039876937, 0.00249420400431065, 0.00255487299572121, 
    0.00260110465436606, 0.00265627234501242, 0.00270746823264184, 
    0.00276599975384698, 0.00281537327265254, 0.00287347647621705, 
    0.00292059327494595, 0.00298236936536788, 0.00303591535641387, 
    0.00308972558755545, 0.00313916650846214, 0.00320635214515423, 
    0.00325302329394591, 0.00331083018521015, 0.00336352798344366, 
    0.00342453605009409, 0.00347526880637, 0.0035368854763696, 
    0.00358183710304246, 0.00364745068552112, 0.0037004303773277, 
    0.00375432538874999, 0.00380325861445653, 0.00387335762552776, 
    0.00391634217992105, 0.00397446942616833, 0.00402701574173548, 
    0.00408742853404312, 0.00413425508849616, 0.00419666340053245, 
    0.00423620758008159, 0.00430204479621642, 0.00435260324304223, 
    0.00440295458555627, 0.00444672794871208, 0.00451583506146998, 
    0.00455367154523901, 0.00460791839884102, 0.00465577390211567, 
    0.00471190786542969, 0.00475240004474876, 0.00481136588279445, 
    0.00484311031093025, 0.00490446054992186, 0.0049491253391303, 
    0.00499105605318403, 0.00502857155107899, 0.00509294364329087, 
    0.00512091139726308, 0.00516700534960704, 0.00520758101301256, 
    0.0052556000547072, 0.0052880394035528, 0.00533783643375657, 
    0.00536006709431809, 0.00541127160794819, 0.00544801684527627, 
    0.0054789096623174, 0.00550603212557578, 0.00556132877034415, 
    0.00557769069921848, 0.00561197107979102, 0.00564406382858764, 
    0.00567776723065662, 0.00570041531165043, 0.00573664047684385, 
    0.00574870039434772, 0.0057864748328538, 0.00581393038362284, 
    0.00582789350429479, 0.00584349425743275, 0.00588520192283833, 
    0.00589120658286085, 0.00590860467565577, 0.00593012838269535, 
    0.00594745869485453, 0.0059594587924201, 0.0059783521771989, 
    0.00597935774864098, 0.00599961662244648, 0.00601564015285493, 
    0.00601270465140753, 0.0060179279930644, 0.00604112699102778, 
    0.0060349395790864, 0.00603342536037229, 0.00604547046914358, 
    0.00604396668880796, 0.00604396668880796, 0.00604547046914358, 
    0.00603342536037229, 0.0060349395790864, 0.00604112699102778, 
    0.0060179279930644, 0.00601270465140753, 0.00601564015285493, 
    0.00599961662244648, 0.00597935774864098, 0.0059783521771989, 
    0.0059594587924201, 0.00594745869485453, 0.00593012838269535, 
    0.00590860467565577, 0.00589120658286085, 0.00588520192283833, 
    0.00584349425743275, 0.00582789350429479, 0.00581393038362284, 
    0.0057864748328538, 0.00574870039434772, 0.00573664047684385, 
    0.00570041531165043, 0.00567776723065662, 0.00564406382858764, 
    0.00561197107979102, 0.00557769069921848, 0.00556132877034415, 
    0.00550603212557578, 0.0054789096623174, 0.00544801684527627, 
    0.00541127160794819, 0.00536006709431809, 0.00533783643375657, 
    0.0052880394035528, 0.0052556000547072, 0.00520758101301256, 
    0.00516700534960704, 0.00512091139726308, 0.00509294364329087, 
    0.00502857155107899, 0.00499105605318403, 0.0049491253391303, 
    0.00490446054992186, 0.00484311031093025, 0.00481136588279445, 
    0.00475240004474876, 0.00471190786542969, 0.00465577390211567, 
    0.00460791839884102, 0.00455367154523901, 0.00451583506146998, 
    0.00444672794871208, 0.00440295458555627, 0.00435260324304223, 
    0.00430204479621642, 0.00423620758008159, 0.00419666340053245, 
    0.00413425508849616, 0.00408742853404312, 0.00402701574173548, 
    0.00397446942616833, 0.00391634217992105, 0.00387335762552776, 
    0.00380325861445653, 0.00375432538874999, 0.0037004303773277, 
    0.00364745068552112, 0.00358183710304246, 0.0035368854763696, 
    0.00347526880637, 0.00342453605009409, 0.00336352798344366, 
    0.00331083018521015, 0.00325302329394591, 0.00320635214515423, 
    0.00313916650846214, 0.00308972558755545, 0.00303591535641387, 
    0.00298236936536788, 0.00292059327494595, 0.00287347647621705, 
    0.00281537327265254, 0.00276599975384698, 0.00270746823264184, 
    0.00265627234501242, 0.00260110465436606, 0.00255487299572121, 
    0.00249420400431065, 0.00244614039876937, 0.00239541632584787, 
    0.00234531358880317, 0.00228872475307247, 0.00224380815805667, 
    0.00219115484337459, 0.00214484907224243, 0.00209187114693067, 
    0.00204512066646243, 0.00199630062260271, 0.00195280787976721, 
    0.00189980407645338, 0.0018562583846562, 0.00181047798514316, 
    0.00176663042920483, 0.00171767989414319, 0.00167717257343993, 
    0.00163147104677363, 0.00159092303933574, 0.00154541778859735, 
    0.00150539796311126, 0.00146345127030891, 0.0014252162125426, 
    0.00138128344043523, 0.0013445000284658, 0.00130536346186148, 
    0.00126878552535795, 0.00122807648012261, 0.00119389480531594, 
    0.00115673808940178, 0.00112304591719274, 0.00108576340161162, 
    0.00105285567928177, 0.00101921268215911, 0.000988031685576328, 
    0.000953170667332374, 0.000923561690550778, 0.000891933506950868, 
    0.000863092267172128, 0.000831468660399712, 0.000804134286587164, 
    0.000775465325936556, 0.000749029914456502, 0.000720100717000519, 
    0.000695245113957912, 0.000669183419369431, 0.000645143879580884, 
    0.000618958220644729, 0.000596600682166158, 0.00057271638744853, 
    0.000551197108116751, 0.000527954945737684, 0.000507307844547428, 
    0.000486249620364204, 0.000467014843254923, 0.000445824728438816, 
    0.000427895849832854, 0.000409016551129646, 0.000391510702150583, 
    0.000373243195663831, 0.000357225262979231, 0.000340230848663388, 
    0.000325053056327858, 0.000308943770625318, 0.000294500496013393, 
    0.000280071215711097, 0.00026678346959845, 0.000252292189916198, 
    0.000240158228749499, 0.000227581278858561, 0.000215648537511137, 
    0.000203714327908276, 0.000193042898276033, 0.000181894744456748, 
    0.000172089241409678, 0.000161791436903044, 0.000152377694230373, 
    0.000143310092776859, 0.000134851358499672, 0.000125959362690513, 
    0.000118326281210706, 0.000110736100319434, 0.000103324794235012, 
    0.0000962794049351589, 0.0000898906011133194, 0.0000833807809254238, 
    0.0000776080593788927, 0.0000717884454243581, 0.0000664096866353811, 
    0.0000614033026607431, 0.0000566944124731823, 0.0000519010058320574, 
    0.0000478252663892545, 0.0000438300859927844, 0.0000400215116856982, 
    0.0000364693200134668, 0.0000332843527908012, 0.0000300917345808219, 
    0.0000273414742040734, 0.0000246266928434587, 0.0000221578327977137, 
    0.0000199177453021699, 0.0000178771455433162, 0.0000158065268432332, 
    0.0000141262232036042, 0.0000124950487673702, 0.0000109976232248058, 
    0.00000963658484739834, 0.00000846519230695818, 0.00000732332546568658, 
    0.00000636729617384379, 0.0000054643879048641, 0.00000466222719446926, 
    0.0000039708280035661, 0.00000336826973731736, 0.00000278841207660652, 
    0.00000233994033597208, 0.00000192783085888245, 0.0000015632561515498, 
    0.00000126104788803202, 0.00000101300448522671, 0.000000791481446243351, 
    0.000000618158480261655, 0.000000474074444808572, 0.000000352002770157532, 
    0.000000260546515506833, 0.000000186927270260604, 0.000000128300289510607, 
    0.0000000874185522002982, 0.0000000573996109710395, 0.0000000344237075387869, 
    0.0000000203949856727634, 0.0000000112183891945797, 0.00000000507006955419654, 
    0.00000000216797091797092, 0.000000000757069209450162, 0.000000000149119692770486, 
    0.0000000000114707455977297), k15 = c(7.64716373181982e-13, 
    0.0000000000107060292245477, 0.0000000000596478771081946, 
    0.000000000188120227802767, 0.000000000464182838521463, 0.00000000106907348970841, 
    0.00000000208232268417454, 0.00000000365381483106351, 0.00000000630814536237817, 
    0.0000000100300199506549, 0.000000015236973735651, 0.0000000227457238039249, 
    0.000000032865215570242, 0.0000000454960359060888, 0.0000000630080408519562, 
    0.0000000841478602721989, 0.000000110874697514909, 0.000000144025152292348, 
    0.000000184929066377479, 0.00000023203330081637, 0.000000293210610670928, 
    0.000000361307074270037, 0.000000442010651997424, 0.000000538241030965899, 
    0.000000651269169787688, 0.00000077254095111238, 0.000000924886217544948, 
    0.00000108866246432384, 0.00000127909366500636, 0.00000149040774040774, 
    0.00000173665405973607, 0.00000199260921813832, 0.00000231068993469787, 
    0.0000026310097407981, 0.00000300633253675582, 0.00000340961948104805, 
    0.00000388128812814659, 0.00000433998259725773, 0.00000491482830685873, 
    0.00000547993000043265, 0.00000614552766090465, 0.00000681699375482444, 
    0.00000761818327542534, 0.00000837784869696245, 0.0000093401066036648, 
    0.0000102280080785372, 0.0000113046644958021, 0.0000123578364732465, 
    0.0000136342803960198, 0.0000147807148881884, 0.0000162547806397013, 
    0.0000175981731867975, 0.0000192443704761826, 0.0000207667173606327, 
    0.000022651425098515, 0.0000243112435159393, 0.0000264931024479636, 
    0.0000283605038896044, 0.0000307159771445486, 0.0000328480828646173, 
    0.0000355370879304411, 0.0000377947730791646, 0.0000408032988224787, 
    0.0000433566760865041, 0.0000466327562061689, 0.0000494251532197961, 
    0.0000530675470118261, 0.0000560759251648802, 0.0000601479480861095, 
    0.0000634220414726367, 0.0000677433443430467, 0.0000713966858146752, 
    0.0000762310598413442, 0.0000800166850100713, 0.0000852775122988417, 
    0.0000894521440983081, 0.0000951499983824719, 0.0000996308172647458, 
    0.000105782007650394, 0.0001105653437416, 0.000117300336959728, 
    0.000122394362019031, 0.000129540404817356, 0.000135069375314084, 
    0.000142890662669929, 0.000148597018306013, 0.00015696776661145, 
    0.000163175491713851, 0.000172183034637565, 0.000178668229428811, 
    0.000188202716993226, 0.000195210462364894, 0.000205552244180716, 
    0.000212765092601402, 0.000223706415877381, 0.000231425654127374, 
    0.000243234915957304, 0.000251253125426406, 0.000263618923361814, 
    0.000272205811102967, 0.000285543701197636, 0.000294328329298567, 
    0.000308341718601309, 0.000317833578580933, 0.000332775242559469, 
    0.000342327330835929, 0.000358212822291857, 0.000368366461703102, 
    0.000385171041297033, 0.000395779592125492, 0.000413119415062206, 
    0.000424329926148709, 0.000443243939151677, 0.000454356259126431, 
    0.00047386423574833, 0.000486011373742855, 0.000506634094245701, 
    0.000518751292064784, 0.000540600744573132, 0.000553193628094422, 
    0.000576040136163317, 0.000589342603066148, 0.000612781636322476, 
    0.000626509190703966, 0.000652058211023985, 0.000665281480840343, 
    0.00069140227959259, 0.000706376569430472, 0.000733475069536577, 
    0.000747880086917785, 0.000776748208771192, 0.00079160918599576, 
    0.000821578612604572, 0.000837284902719757, 0.000867575339673554, 
    0.000883464165292869, 0.00091677539939362, 0.000931834614043609, 
    0.00096519759726605, 0.000982585632511228, 0.00101683204973549, 
    0.00103327687921769, 0.00107008968764921, 0.00108641383265226, 
    0.00112422474812934, 0.00114167397062569, 0.00117949607925633, 
    0.00119710329903782, 0.00123868711403847, 0.00125469277531943, 
    0.00129578234265734, 0.00131504499903144, 0.00135707694623865, 
    0.00137431624109038, 0.00141955161883237, 0.00143627528684572, 
    0.0014823790298534, 0.00150109156073938, 0.00154636337721655, 
    0.0015645853784908, 0.00161459065549261, 0.00163040647440449, 
    0.00167976422705259, 0.00169957720544691, 0.00174923382378326, 
    0.00176609732095016, 0.00182009290098741, 0.00183616852999293, 
    0.00189022498451235, 0.00190873401443011, 0.00196112904768659, 
    0.00197878255116185, 0.00203766705473881, 0.00205149872907809, 
    0.00210861674930375, 0.00212744719792504, 0.00218482933971525, 
    0.00220001175858484, 0.00226198514812519, 0.0022756253596614, 
    0.00233708420826063, 0.00235417629750467, 0.00241356712973363, 
    0.00242868469530176, 0.00249567129620453, 0.00250557630261665, 
    0.0025701831398136, 0.00258684577351905, 0.00265037998741289, 
    0.00266199899239747, 0.00273137503831535, 0.00274089258435323, 
    0.00280956747176985, 0.00282278984151504, 0.00288805892385588, 
    0.00289843755200071, 0.00297289816955409, 0.00297758552685801, 
    0.00304794507526816, 0.00306037199468912, 0.00312906683049706, 
    0.00313537529398144, 0.00321138864136549, 0.00321426292879666, 
    0.00328859753256248, 0.00329589247710941, 0.00336598205234531, 
    0.00337038421632502, 0.00345051161926575, 0.00344739486703772, 
    0.00352201314998934, 0.00352857096607097, 0.0036015672267119, 
    0.00360002778459293, 0.00368086478315066, 0.00367483693310413, 
    0.00375368112361747, 0.00375368945061405, 0.00382639581798395, 
    0.00382235852771567, 0.00390615260189913, 0.00389420273625796, 
    0.0039721398982105, 0.00396992517482762, 0.00404497299590719, 
    0.00403354747410634, 0.00411775147526636, 0.00410201405431233, 
    0.00418245658463499, 0.00417333054279152, 0.00424642463359808, 
    0.0042328239548395, 0.00431917741160466, 0.00429549052841877, 
    0.00437430109588261, 0.00436179227916166, 0.00443734223060033, 
    0.00441558934953446, 0.00449983659464066, 0.00447283947680773, 
    0.00455277118806385, 0.00453331337454188, 0.00460551097840284, 
    0.00458042732393758, 0.00466622231368842, 0.00463009698451167, 
    0.00470797155672321, 0.00468511803391102, 0.00475802009984798, 
    0.00472458374438368, 0.00480642057365768, 0.00476775563440577, 
    0.00484538849506607, 0.00481456111404028, 0.00488313540301801, 
    0.00484582058734671, 0.00492936068775189, 0.00488162413993267, 
    0.00495488599192303, 0.00492057251366114, 0.00498828318396771, 
    0.0049439062462376, 0.00502188199807247, 0.00497089721345674, 
    0.00504366917207939, 0.00500092558669774, 0.00506406921635096, 
    0.00501616535164816, 0.00509325113192094, 0.0050341453905442, 
    0.00510044401971485, 0.00505612382852793, 0.00511742868389694, 
    0.00506151447177406, 0.00513263486479839, 0.00507024930383892, 
    0.0051360578354626, 0.00508359645694302, 0.00513813142355636, 
    0.00507987849464371, 0.00514854587713384, 0.00507987849464371, 
    0.00513813142355636, 0.00508359645694302, 0.0051360578354626, 
    0.00507024930383892, 0.00513263486479839, 0.00506151447177406, 
    0.00511742868389694, 0.00505612382852793, 0.00510044401971485, 
    0.0050341453905442, 0.00509325113192094, 0.00501616535164816, 
    0.00506406921635096, 0.00500092558669774, 0.00504366917207939, 
    0.00497089721345674, 0.00502188199807247, 0.0049439062462376, 
    0.00498828318396771, 0.00492057251366114, 0.00495488599192303, 
    0.00488162413993267, 0.00492936068775189, 0.00484582058734671, 
    0.00488313540301801, 0.00481456111404028, 0.00484538849506607, 
    0.00476775563440577, 0.00480642057365768, 0.00472458374438368, 
    0.00475802009984798, 0.00468511803391102, 0.00470797155672321, 
    0.00463009698451167, 0.00466622231368842, 0.00458042732393758, 
    0.00460551097840284, 0.00453331337454188, 0.00455277118806385, 
    0.00447283947680773, 0.00449983659464066, 0.00441558934953446, 
    0.00443734223060033, 0.00436179227916166, 0.00437430109588261, 
    0.00429549052841877, 0.00431917741160466, 0.0042328239548395, 
    0.00424642463359808, 0.00417333054279152, 0.00418245658463499, 
    0.00410201405431233, 0.00411775147526636, 0.00403354747410634, 
    0.00404497299590719, 0.00396992517482762, 0.0039721398982105, 
    0.00389420273625796, 0.00390615260189913, 0.00382235852771567, 
    0.00382639581798395, 0.00375368945061405, 0.00375368112361747, 
    0.00367483693310413, 0.00368086478315066, 0.00360002778459293, 
    0.0036015672267119, 0.00352857096607097, 0.00352201314998934, 
    0.00344739486703772, 0.00345051161926575, 0.00337038421632502, 
    0.00336598205234531, 0.00329589247710941, 0.00328859753256248, 
    0.00321426292879666, 0.00321138864136549, 0.00313537529398144, 
    0.00312906683049706, 0.00306037199468912, 0.00304794507526816, 
    0.00297758552685801, 0.00297289816955409, 0.00289843755200071, 
    0.00288805892385588, 0.00282278984151504, 0.00280956747176985, 
    0.00274089258435323, 0.00273137503831535, 0.00266199899239747, 
    0.00265037998741289, 0.00258684577351905, 0.0025701831398136, 
    0.00250557630261665, 0.00249567129620453, 0.00242868469530176, 
    0.00241356712973363, 0.00235417629750467, 0.00233708420826063, 
    0.0022756253596614, 0.00226198514812519, 0.00220001175858484, 
    0.00218482933971525, 0.00212744719792504, 0.00210861674930375, 
    0.00205149872907809, 0.00203766705473881, 0.00197878255116185, 
    0.00196112904768659, 0.00190873401443011, 0.00189022498451235, 
    0.00183616852999293, 0.00182009290098741, 0.00176609732095016, 
    0.00174923382378326, 0.00169957720544691, 0.00167976422705259, 
    0.00163040647440449, 0.00161459065549261, 0.0015645853784908, 
    0.00154636337721655, 0.00150109156073938, 0.0014823790298534, 
    0.00143627528684572, 0.00141955161883237, 0.00137431624109038, 
    0.00135707694623865, 0.00131504499903144, 0.00129578234265734, 
    0.00125469277531943, 0.00123868711403847, 0.00119710329903782, 
    0.00117949607925633, 0.00114167397062569, 0.00112422474812934, 
    0.00108641383265226, 0.00107008968764921, 0.00103327687921769, 
    0.00101683204973549, 0.000982585632511228, 0.00096519759726605, 
    0.000931834614043609, 0.00091677539939362, 0.000883464165292869, 
    0.000867575339673554, 0.000837284902719757, 0.000821578612604572, 
    0.00079160918599576, 0.000776748208771192, 0.000747880086917785, 
    0.000733475069536577, 0.000706376569430472, 0.00069140227959259, 
    0.000665281480840343, 0.000652058211023985, 0.000626509190703966, 
    0.000612781636322476, 0.000589342603066148, 0.000576040136163317, 
    0.000553193628094422, 0.000540600744573132, 0.000518751292064784, 
    0.000506634094245701, 0.000486011373742855, 0.00047386423574833, 
    0.000454356259126431, 0.000443243939151677, 0.000424329926148709, 
    0.000413119415062206, 0.000395779592125492, 0.000385171041297033, 
    0.000368366461703102, 0.000358212822291857, 0.000342327330835929, 
    0.000332775242559469, 0.000317833578580933, 0.000308341718601309, 
    0.000294328329298567, 0.000285543701197636, 0.000272205811102967, 
    0.000263618923361814, 0.000251253125426406, 0.000243234915957304, 
    0.000231425654127374, 0.000223706415877381, 0.000212765092601402, 
    0.000205552244180716, 0.000195210462364894, 0.000188202716993226, 
    0.000178668229428811, 0.000172183034637565, 0.000163175491713851, 
    0.00015696776661145, 0.000148597018306013, 0.000142890662669929, 
    0.000135069375314084, 0.000129540404817356, 0.000122394362019031, 
    0.000117300336959728, 0.0001105653437416, 0.000105782007650394, 
    0.0000996308172647458, 0.0000951499983824719, 0.0000894521440983081, 
    0.0000852775122988417, 0.0000800166850100713, 0.0000762310598413442, 
    0.0000713966858146752, 0.0000677433443430467, 0.0000634220414726367, 
    0.0000601479480861095, 0.0000560759251648802, 0.0000530675470118261, 
    0.0000494251532197961, 0.0000466327562061689, 0.0000433566760865041, 
    0.0000408032988224787, 0.0000377947730791646, 0.0000355370879304411, 
    0.0000328480828646173, 0.0000307159771445486, 0.0000283605038896044, 
    0.0000264931024479636, 0.0000243112435159393, 0.000022651425098515, 
    0.0000207667173606327, 0.0000192443704761826, 0.0000175981731867975, 
    0.0000162547806397013, 0.0000147807148881884, 0.0000136342803960198, 
    0.0000123578364732465, 0.0000113046644958021, 0.0000102280080785372, 
    0.0000093401066036648, 0.00000837784869696245, 0.00000761818327542534, 
    0.00000681699375482444, 0.00000614552766090465, 0.00000547993000043265, 
    0.00000491482830685873, 0.00000433998259725773, 0.00000388128812814659, 
    0.00000340961948104805, 0.00000300633253675582, 0.0000026310097407981, 
    0.00000231068993469787, 0.00000199260921813832, 0.00000173665405973607, 
    0.00000149040774040774, 0.00000127909366500636, 0.00000108866246432384, 
    0.000000924886217544948, 0.00000077254095111238, 0.000000651269169787688, 
    0.000000538241030965899, 0.000000442010651997424, 0.000000361307074270037, 
    0.000000293210610670928, 0.00000023203330081637, 0.000000184929066377479, 
    0.000000144025152292348, 0.000000110874697514909, 0.0000000841478602721989, 
    0.0000000630080408519562, 0.0000000454960359060888, 0.000000032865215570242, 
    0.0000000227457238039249, 0.000000015236973735651, 0.0000000100300199506549, 
    0.00000000630814536237817, 0.00000000365381483106351, 0.00000000208232268417454, 
    0.00000000106907348970841, 0.000000000464182838521463, 0.000000000188120227802767, 
    0.0000000000596478771081946, 0.0000000000107060292245477, 
    7.64716373181982e-13)), .Names = c("", "", "k3", "k4", "k5", 
"k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15"
))
.Random.seed <-
c(403L, 10L, -1874989363L, 601474695L, -254291602L, 1551838024L, 
-1542885557L, -1416350039L, 1161235448L, -1850985690L, 1933210689L, 
-169241133L, -287664254L, -1083384908L, 337457975L, -789917139L, 
834720580L, -554387350L, -976657707L, -1210966929L, 1282479478L, 
530554528L, 64559971L, 335474689L, 1782258544L, -1393601778L, 
-1190122119L, -308410101L, 1810430522L, -2086555332L, -1250484609L, 
352683477L, 1540029420L, 721681730L, -694779235L, 926232343L, 
-387297666L, 1829531192L, 441997595L, -1222771079L, -321799480L, 
-395121930L, -1243126991L, -2131315869L, 630819122L, 334625412L, 
-1806084409L, 457547165L, 875067668L, 609851322L, -1483783675L, 
9147359L, 2004576870L, -477600272L, -772070637L, -452096847L, 
-1825566880L, -434598658L, 28118153L, 461646779L, 1763083658L, 
-749239188L, 1288183279L, -603777499L, 907293404L, 475916466L, 
-2041664595L, 963506023L, -1898726258L, -372098712L, 1017014635L, 
-1241879799L, 179092888L, 682077382L, -1462684127L, 1669207731L, 
-1965409246L, -62175724L, 1842100887L, -1104612211L, 2009627044L, 
1361863562L, 119132149L, -1903641009L, 1725548886L, -863341568L, 
723647811L, 833122529L, -2008610224L, 785069742L, -1980464551L, 
162900587L, 1758339034L, -1806344036L, 492180127L, 2114896117L, 
649578892L, 1218208866L, 1320657981L, 1576781495L, -1430657698L, 
541043864L, -580651013L, -1016742503L, 1621605928L, 324486870L, 
1342935889L, 1358345731L, -565118574L, 2103904036L, -895981081L, 
-1953708483L, -1562578764L, -389454054L, -1310372763L, 831276287L, 
-1689180794L, -1949705456L, 1622843315L, 1174873169L, 560613376L, 
1310841054L, -1291611351L, 1228705499L, 1243815402L, -1389107828L, 
-377537841L, 183535237L, 148481340L, 628582418L, 1104824077L, 
-1009474489L, -332102098L, 671248264L, -999675253L, -1490699159L, 
-313843656L, 1357900006L, 1452937345L, -635328493L, 820494274L, 
168904820L, -476635529L, 867467117L, -2009202684L, -441779414L, 
710172821L, 929226543L, 777938486L, -1601715616L, -39173981L, 
1807045569L, -1858232400L, -2033259826L, -495003591L, -1871520181L, 
1163125754L, -1259729156L, 1629831999L, -268956651L, 1752622892L, 
1644022786L, -587010339L, 1355136471L, -1849550658L, 992234232L, 
1593561307L, 97047353L, 1268389512L, -1888853450L, 1595095665L, 
1483030691L, -1992816142L, 2033903940L, 1675313671L, 1476775901L, 
-875849516L, 256468986L, 899817285L, 997346207L, 2029060390L, 
-821071440L, -1215157677L, 297768305L, -2146601824L, 454743102L, 
-1262314679L, 1803799035L, -1921929782L, 29221676L, -1838183633L, 
153506277L, -96969572L, -1629652750L, 831009133L, 976518823L, 
-806571826L, -1797202392L, -666393045L, 1433939017L, -389845672L, 
992334854L, -1860200991L, 791281523L, 579951586L, 489091924L, 
1184210519L, -1174288819L, -939542044L, -81030198L, -551709131L, 
-922305393L, 395744662L, 395782464L, 1686348803L, 1176327713L, 
-1255325424L, 596881134L, -378711399L, -1078547669L, -1860274662L, 
1010638044L, 617418975L, 1556692661L, -151013300L, -1011956574L, 
-51489795L, -974876425L, -798738914L, 1138870516L, 756338066L, 
1728680288L, 559271948L, 1483765472L, -1065342366L, -43784536L, 
-1796569524L, -1408408708L, -1853102798L, 1688401520L, -1572304060L, 
-1305181224L, -1437937798L, -641496528L, -1779962948L, 1901318740L, 
-1616935870L, -1703706464L, 731200188L, -2144895536L, 1694975842L, 
-661042456L, -362687284L, 1691422204L, -1886799374L, -2096228736L, 
-245950988L, 383234696L, 1261477690L, -1089339568L, 471333996L, 
802977172L, 1304085586L, 1855199712L, 1312517324L, -2106120032L, 
-2079225470L, 1322768616L, -1226335508L, 681166140L, -1468152718L, 
-1084617616L, 48337956L, 590827576L, 1322935066L, -1028496176L, 
469963580L, 54206356L, 892213122L, -1881969216L, -302648772L, 
467696016L, -1858579934L, 1614482696L, -505912692L, -1465317604L, 
1098606738L, -404428416L, -1632065004L, -375594200L, -1879482822L, 
-1842818608L, 1654764908L, 33516468L, 210897938L, 1211111904L, 
-1126573108L, -230946592L, -2075852766L, -364835544L, 359738892L, 
1575844668L, 1986692082L, 293756656L, -80010684L, -1163071912L, 
-996834502L, -1908088336L, 960720572L, -1881264876L, -1829226814L, 
-95881120L, -974841988L, -342705200L, 1828384034L, 1537312424L, 
993041292L, 529165500L, 192497458L, -1572455872L, -293277516L, 
-904603064L, 1338887610L, -1315778032L, 521434860L, -993458860L, 
1003697426L, 1702767008L, 1772762828L, 1537056352L, -1180873790L, 
-308942040L, 1172482988L, -1824817348L, 1753185650L, 1447423792L, 
1641493796L, 238211896L, -188199206L, 375921552L, -457864452L, 
787010452L, 1966530626L, 717820160L, -586818500L, -581380784L, 
2132594850L, -475757240L, 30087948L, 80304860L, 36708690L, -22009728L, 
-1673909164L, 1033442280L, 1428942074L, -2142589104L, 429160236L, 
-244916876L, -1245462638L, 771501152L, 1170359692L, 308626272L, 
-274684446L, 1513034664L, 1647335628L, -961637380L, -1118391886L, 
-375210512L, 690659268L, 571476312L, 1832209786L, 2116155312L, 
-1910841924L, -406432684L, 1810696130L, -1317574624L, -1091026756L, 
205391440L, 1609086946L, 1608341608L, -319498164L, 1289446652L, 
-80193550L, -516096768L, -353774220L, -1360445432L, -2089331782L, 
-417016368L, 549736428L, 782624148L, 1175708114L, 1083332960L, 
1599331148L, -1876250336L, 1072105346L, -2008580376L, -1097488148L, 
-672724932L, 604847986L, 2110541424L, 440777764L, -2088841416L, 
565886106L, 951304912L, 746442940L, -1184760172L, 747531906L, 
-971996992L, -243620036L, 2018996368L, 811335202L, 834006024L, 
881798412L, 473447580L, 1251062290L, 2088244096L, 613819924L, 
154140200L, 1158462778L, 2054614480L, -764790676L, -1792785100L, 
88525458L, 13762400L, -1840836788L, 1780996064L, 1086131106L, 
672742312L, 1927801228L, 589458620L, 2025849202L, -35743504L, 
1187010628L, -1494489768L, -498431430L, 1297239920L, -585298884L, 
969323284L, -133351998L, -350421280L, 487336828L, -487406896L, 
1013374114L, 1230790696L, 1664727308L, 1932032956L, -800707278L, 
1754620352L, 1674615348L, -1707905080L, 1144204474L, 718090640L, 
-567897620L, 36283988L, -383994734L, 1235206944L, 551363040L, 
-1934830519L, 1612219003L, -1578972340L, 1570995930L, 1881805519L, 
-1645972839L, 1319837358L, -321864772L, 1189807149L, 1137855447L, 
936403488L, -1881840770L, 363670859L, -1106669747L, 1751508346L, 
1032378296L, 1980468577L, -2036547309L, -1589823180L, -1811549374L, 
-747347113L, -1175854431L, 1533154150L, 2036608356L, 197908629L, 
-1874404929L, 1531743576L, 1359967094L, -1638723197L, -580693051L, 
-801928990L, 1358867792L, -132121479L, 1900777387L, -506727140L, 
-978779574L, 488179263L, 203070089L, 49364638L, 1414077260L, 
1569241885L, -987641177L, -18415152L, 710224238L, -360164325L, 
-2131624579L, -1651016854L, 1176174088L, -636210511L, -1042744029L, 
310169124L, 759899346L, 1342760231L, 1696984945L, 895790518L, 
-884501164L, -1380772891L, -964178449L, 773641512L, -1672977018L, 
977617331L, -87646955L, -1764262030L, 1421528000L, -1480467415L, 
1607248027L, 1577661804L, -1246515846L, 575135407L, -1694104135L, 
-1784961074L, 1841743644L, -1462667635L, 1530541815L, 952711680L, 
-2025290658L, 248252203L, -1027562451L, -1558252390L, -1501185960L, 
298315713L, -2100002701L, 597014932L, -1587398622L, -1999851721L, 
1797698049L, -1935480058L, 1905243972L, -1868215691L, -1378669601L, 
711946040L, -1380746986L, -1323113181L, -1092233499L, -191320318L, 
-765966992L, -1871858407L, 1749699851L, -803287556L, 1225713194L, 
904457823L, -1995108887L, -1319399682L, -496955668L, -1099727299L, 
557272583L, -487693840L, 1755406734L, 2060429243L, 250614173L, 
1240633162L, 1268577128L, -1466342063L, 1201989187L, 1777142596L, 
-1798907918L, -1705495097L, -1668211823L, -1390832874L, 1773119348L, 
-1892010491L, -291848625L, 2007030344L, 1323764710L, -1222859501L, 
-346916235L, -632928174L, -1734907232L, 2020800393L, 1856930747L, 
-1758133236L, 1592844698L, 649863439L, 1093259097L, -510606866L, 
-1972999044L, 1429215469L, -329986153L, -861013920L, 1183603262L, 
-1595077877L, 11211149L, 1580866234L, -1131294472L, -375894367L, 
-767859117L, 149330292L, 224517890L, -1745078377L, 1298889057L, 
695370150L, 1168544676L, 1425436245L, -1705816321L, 1266119576L, 
-345332554L, 1535521091L, -1308923771L, -758116062L, 49734160L, 
-1569068615L, 615631467L, -677108388L, -9923446L, -10191745L, 
-1859938871L, 1199369822L, -754684660L, -182748981L)
.txtline <-
function(txt, width, space="", ind="") {
  paste(
    ind, paste(format(names(txt), width=width, justify="right"), collapse=space), "\n",
    ind, paste(format(txt, width=width, justify="right"), collapse=space), "\n",
    sep="" )
}
.WrdPrepRep <-
function(wrd, main="Bericht" ){
  
  # only internal user out from GetNewWrd()
  # creates new word instance and prepares document for report
  
  # constants
  # wdPageBreak <- 7
  # wdSeekCurrentPageHeader <- 9  ### Kopfzeile
  # wdSeekCurrentPageFooter <- 10	### Fusszeile
  # wdSeekMainDocument <- 0
  # wdPageFitBestFit <- 2
  # wdFieldEmpty <- -1
  
  # Show DocumentMap
  wrd[["ActiveWindow"]][["DocumentMap"]] <- TRUE
  wrdWind <- wrd[["ActiveWindow"]][["ActivePane"]][["View"]][["Zoom"]]
  wrdWind[["PageFit"]] <- wdConst$wdPageFitBestFit
  
  wrd[["Selection"]]$TypeParagraph()
  wrd[["Selection"]]$TypeParagraph()
  
  wrd[["Selection"]]$WholeStory()
  # 15.1.2012 auskommentiert: WrdSetFont(wrd=wrd)
  
  # Idee: ueberschrift definieren (geht aber nicht!)
  #wrd[["ActiveDocument"]][["Styles"]]$Item("ueberschrift 2")[["Font"]][["Name"]] <- "Consolas"
  #wrd[["ActiveDocument"]][["Styles"]]$Item("ueberschrift 2")[["Font"]][["Size"]] <- 10
  #wrd[["ActiveDocument"]][["Styles"]]$Item("ueberschrift 2")[["Font"]][["Bold"]] <- TRUE
  
  #wrd[["ActiveDocument"]][["Styles"]]$Item("ueberschrift 2")[["ParagraphFormat"]]["Borders"]]$Item(wdBorderTop)[["LineStyle"]] <- wdConst$wdLineStyleSingle
  
  WrdCaption( main, wrd=wrd)
  wrd[["Selection"]]$TypeText(gettextf("%s/%s\n",format(Sys.time(), "%d.%m.%Y"), Sys.getenv("username")))
  wrd[["Selection"]]$InsertBreak( wdConst$wdPageBreak)
  
  # Inhaltsverzeichnis einfuegen ***************
  wrd[["ActiveDocument"]][["TablesOfContents"]]$Add( wrd[["Selection"]][["Range"]] )
  # Original VB-Code:
  # With ActiveDocument
  # .TablesOfContents.Add Range:=Selection.Range, RightAlignPageNumbers:= _
  # True, UseHeadingStyles:=True, UpperHeadingLevel:=1, _
  # LowerHeadingLevel:=2, IncludePageNumbers:=True, AddedStyles:="", _
  # UseHyperlinks:=True, HidePageNumbersInWeb:=True, UseOutlineLevels:= _
  # True
  # .TablesOfContents(1).TabLeader = wdTabLeaderDots
  # .TablesOfContents.Format = wdIndexIndent
  # End With
  
  # Fusszeile	***************
  wrdView <- wrd[["ActiveWindow"]][["ActivePane"]][["View"]]
  wrdView[["SeekView"]] <- wdConst$wdSeekCurrentPageFooter
  wrd[["Selection"]]$TypeText( gettextf("%s/%s\t\t",format(Sys.time(), "%d.%m.%Y"), Sys.getenv("username")) )
  wrd[["Selection"]][["Fields"]]$Add( wrd[["Selection"]][["Range"]], wdConst$wdFieldEmpty, "PAGE" )
  # Roland wollte das nicht (23.11.2014):
  # wrd[["Selection"]]$TypeText("\n\n")
  wrdView[["SeekView"]] <- wdConst$wdSeekMainDocument
  
  wrd[["Selection"]]$InsertBreak( wdConst$wdPageBreak)
  invisible()
  
}
.writeCB <-
function (dat, ...) {
  
  sn <- Sys.info()["sysname"]
  if (sn == "Darwin") {
    file <- pipe("pbcopy")
    cat(dat, file = file, ...)
    close(file)
  }
  else if (sn == "Windows") {
    cat(dat, file = "clipboard", ...)
  }
  else {
    stop("Writing to the clipboard is not implemented for your system (", 
         sn, ") in this package.")
  }
}
.YuenTTestB <-
function(x, y, trim = 0, conf.level = 0.95, nboot=599 
                       , alternative = c("two.sided", "less", "greater"), mu = 0, na.rm = FALSE){

  
  TrimSE <- function(x, trim = 0, na.rm = FALSE) {

    #  Estimate the standard error of the gamma trimmed mean
    #  The default amount of trimming is trim = 0.2
    
    if(na.rm) x <- na.omit(x)
    
    winvar <- var(Winsorize(x, probs = c(trim, 1-trim)))
    
    trimse <- sqrt(winvar) / ((1 - 2 * trim) * sqrt(length(x)))
    trimse
  }

  
  alternative <- match.arg(alternative)
  method <- "Yuen Two Sample bootstrap t-test"
  dname <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))

  if(na.rm) x <- na.omit(x)
  if(na.rm) y <- na.omit(y)
  
  meanx <- mean(x, trim = trim)
  meany <- mean(y, trim = trim)
  
  tstat <- (meanx - meany ) / sqrt(TrimSE(x, trim = trim)^2 + TrimSE(y, trim = trim)^2)

  sampx <- matrix(sample(x - meanx, size=length(x) * nboot, replace=TRUE), nrow=nboot)
  sampy <- matrix(sample(y - meany, size=length(y) * nboot, replace=TRUE), nrow=nboot)
  
  top <- apply(sampx, 1, mean, trim) - apply(sampy, 1, mean, trim)
  botx <- apply(sampx, 1, TrimSE, trim)
  boty <- apply(sampy, 1, TrimSE, trim)
  tval <- top / sqrt(botx^2 + boty^2)


  alpha <- 1 - conf.level
  se <- sqrt((TrimSE(x, trim = trim))^2 + (TrimSE(y, trim = trim))^2)

  if(alternative == "two.sided") {
    tval <- abs(tval)
    icrit <- floor((1 - alpha) * nboot + .5)
    cint <- meanx - meany + c(-1, 1) * tval[icrit] * se
    pval <- (sum(abs(tstat) <= abs(tval))) / nboot
    
  } else {
    tval <- sort(tval)
    ibot <- floor(alpha/2 * nboot + .5)
    itop <- floor((1 - alpha/2) * nboot + .5)
    cint <- meanx - meany - tval[c(itop, ibot)] * se
    
  }  

  names(tstat) <- "t"
  names(mu) <- "difference in means"
  estimate <- c(meanx, meany)
  names(estimate) <- c("mean of x", "mean of y")

  attr(cint, "conf.level") <- conf.level
  rval <- list(statistic = tstat, p.value = pval, 
               conf.int = cint, estimate = estimate, null.value = mu, 
               alternative = alternative, method = method, data.name = dname)
  class(rval) <- "htest"
  return(rval)

}
