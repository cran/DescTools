Desc.Date <-
function(x, main = NULL, maxrows = 10, digits = 3, dprobs = NULL, mprobs=NULL, plotit=getOption("plotit", FALSE), ... ) {
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

  # old code, replaced 24.11.2012:
  # cat( "\n\n"
    # , format( c("length", "n", "NAs", ifelse(class(x)=="factor","levels","unique")), width=8, justify="right" ), "\n"
    # , format( c(length(x), length(na.omit(x)), sum(is.na(x)), ifelse(class(x)=="factor",length(levels(x)),length(na.omit(unique(x))))), width=8, justify="right" ), "\n\n"
  # , sep="")
  
  if(n==nan) return()
  
  cat(HighLow(x, nlow=4, na.rm=FALSE), "\n", sep="") # set na.rm to FALSE as we have already omitted NAs
    
  # weekdays in your current locale, Sunday : Saturday
  # format(ISOdate(2000, 1, 2:8), "%A")
  xd <- Weekday(x, fmt="ddd") # factor(format(x,"%A"), levels=format(ISOdate(2000, 1, 3:9), "%A"))
  if(is.null(dprobs)) dprobs <- rep(1/7, 7)
  r.chisq <- chisq.test(table(xd), p = dprobs)

  x.frq <- Freq(xd)
  .CaptOut(print(x.frq, digits=digits), file="NUL")

  x.frq[,c(3,5)] <- lapply(x.frq[,c(3,5)], round, digits=digits)
  ftab <- cbind( x.frq, exp=round(r.chisq$exp[], digits=1), res=round(r.chisq$res[], digits=1) )
  cat("\nWeekdays:\n")
  cat(gsub(pattern=" 0\\.", replacement="  \\.", x=.CaptOut(ftab)), sep="\n")
  cat(.CaptOut(r.chisq), sep="\n")

  # months in your current locale
  # format(ISOdate(2000, 1:12, 1), "%B")
  xd <- factor(format(x,"%B"), levels=format(ISOdate(2000, 1:12, 1), "%B"))
  tab <- table(xd)
  
  if(is.null(mprobs)) mprobs <- prop.table( table(factor(months(seq(from=ISOdate(2000,1,1), to=ISOdate(2000,12,31), by="day")),
                                                         , levels=format(ISOdate(2000, 1:12, 1), "%B")  )))
  r.chisq <- chisq.test( x=tab, p = mprobs)

  x.frq <- Freq(xd)
  .CaptOut(print(x.frq, digits=digits), file="NUL")
  x.frq[,c(3,5)] <- lapply(x.frq[,c(3,5)], round, digits=digits)
  ftab <- cbind( x.frq
         , exp=round(r.chisq$exp[], digits=1), prs.res=round(r.chisq$res[], digits=1)
         )
  cat("\nMonths:\n")
  cat(gsub(pattern=" 0\\.", replacement="  \\.", x=.CaptOut(ftab)), sep="\n")
  cat(.CaptOut(r.chisq), sep="\n")

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

  # hist(x=vars0$Birthday, breaks=hbreaks) <-  kommt zu PlotDesc.Date
  if(!is.null(hbreaks)){ 
    cat("\nTable by", hbreaks, ":\n")
    cat( gsub(pattern=" 0\\.", replacement="  \\.", x=.CaptOut(
      Freq(x=x, breaks=hbreaks)) ), "\n", sep="\n")
  } else {
    cat("Warning:\n  No plausible breaks for years found!\n")
  }  
  cat("\n")
  # Die Breaks werden nicht gut dargestellt: What now?
  # Freq( as.integer(format(x,"%Y")), breaks=as.integer(format(hbreaks,"%Y")))
  
  if(plotit) PlotDesc.Date(x, main=main)
  invisible()
  
}
