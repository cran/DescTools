DescWrd.formula <-
function(formula, data = parent.frame(), subset = TRUE, wrd = NULL, ...) {
  
  if(!IsValidWrd(wrd)) stop("wrd is not a valid handle to a running Word instance.")
  
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
      
#       cat( paste(rep("-",(as.numeric(options("width"))-2)), collapse=""), "\n" ) 
#       cat( paste(resp, " ~ ", pred, sep="") )
#       if( !is.null(attr(x,"label")) ) cat(" :", strwrap(attr(x,"label"), indent=2, exdent=2), sep="\n" )
#       cat("\n")

    # coerce logicals and characters to factors
#     if( class(x)[1] %in% c("logical","character")) x <- factor(x)
#     if( class(grp)[1] %in% c("logical","character")) grp <- factor(grp)
    if( IsDichotomous(x)) x <- factor(x)
    if( IsDichotomous(grp)) grp <- factor(grp)

    # main report caption
    WrdCaption( x=paste(resp, " ~ ", pred, sep=""), wrd=wrd )
    
    # coerce logicals and characters to factors
    if( class(x)[1] %in% c("logical","character")) x <- factor(x)
    if( class(grp)[1] %in% c("logical","character")) grp <- factor(grp)


    if(class(x)[1] %in% c("numeric","integer")){
      
      if(class(grp)[1] %in% c("numeric","integer")){
        WrdText( .CaptOut(
          do.call( DescNumNum, args=append( list(x=grp, y=x, xname=pred, yname=resp, plotit=TRUE), dotargs.numeric.numeric)) ))
        WrdPlot(width=13, height=6.5, dfact=2.5, crop=c(0,0,0.2,0), wrd=wrd, append.cr=TRUE)
        
      } else if(class(grp)[1] %in% c("factor","ordered")){
        WrdText( .CaptOut(
          do.call( DescNumFact, args=append( list(x=x, grp=grp, xname=resp, grpname=pred, plotit=TRUE), dotargs.numeric.factor )) ))
        WrdPlot(width=13, height=6.5, dfact=2.5, crop=c(0,0,0.2,0), wrd=wrd, append.cr=TRUE)
        
      } else {
        cat(gettextf("Don't know how to describe class: %s ~ %s!\n", paste(class(x), collapse=", "), 
                     paste(class(grp), collapse=", ")), "\n")
      }  
    } else if(class(x)[1] %in% c("factor","ordered")){
      
      if( class(grp)[1] %in% c("numeric","integer")){
        WrdText( .CaptOut(
          do.call( DescFactNum, args=append( list(x=x, y=grp, xname=resp, yname=pred, plotit=TRUE), dotargs.factor.numeric )) ))
        WrdPlot(width=13, height=6.5, dfact=2.5, crop=c(0,0,0.2,0), wrd=wrd, append.cr=TRUE)
        
      } else if ( class(grp)[1] %in% c("factor","ordered")){
        tab <- table(x, grp, dnn=c(resp, pred))
        WrdText( .CaptOut(
          do.call( Desc, args=append( list(x=tab, 
                                           xname="", grpname="", plotit=FALSE, 
                                           main=NA), dotargs.factor.factor) )
        ))
        WrdText("\n", wrd=wrd)
        PlotDesc.table(tab, horiz = TRUE, wrd=wrd)
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
