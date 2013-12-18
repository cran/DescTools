Desc.formula <-
function(formula, data = parent.frame(), subset = TRUE, ...) {

  mf <- match.call(expand.dots = FALSE) 

  # parse dots.arguments, such as not to send unappropriate arguments to subfunctions
  dotargs.factor.factor <- mf$...[ names(mf$...)[
    !is.na(match(names(mf$...), names( formals( DescFactFact) )))
    ] ]
  dotargs.numeric.factor <- mf$...[ names(mf$...)[
    !is.na(match(names(mf$...), names( formals( DescNumFact) )))
    ] ]
  dotargs.numeric.numeric <- mf$...[ names(mf$...)[
    !is.na(match(names(mf$...), names( formals( DescNumNum) )))
    ] ]
    
  mm <- ParseFormula(formula=formula, data=data, subset=subset)
  
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
#      if( !is.null(attr(x,"label")) ) cat(" :\n", attr(x,"label") )
     if( !is.null(attr(x,"label")) ) cat(" :", strwrap(attr(x,"label"), indent=2, exdent=2), sep="\n" )
      cat("\n")
      
      # coerce logicals and characters to factors
      if( class(x)[1] %in% c("logical","character")) x <- factor(x)
      if( class(grp)[1] %in% c("logical","character")) grp <- factor(grp)
      
      switch( class(x)[1]
        , "numeric" = switch( class(grp)[1]
            , "numeric" = { do.call( DescNumNum, args=append( list(x=grp, y=x, xname=pred, yname=resp), dotargs.numeric.numeric)) }                        
            , "integer" = { do.call( DescNumNum, args=append( list(x=grp, y=x, xname=pred, yname=resp), dotargs.numeric.numeric )) }                        
            , "factor"  = { do.call( DescNumFact, args=append( list(x=x, grp=grp), dotargs.numeric.factor )) }                        
            , "ordered"  = { do.call( DescNumFact, args=append( list(x=x, grp=grp), dotargs.numeric.factor )) }
            , cat(gettextf("Don't know how to describe class: %s ~ %s!\n", paste(class(x), collapse=", "), paste(class(grp), collapse=", ")), "\n")
            
          )
        , "integer" =  switch( class(grp)[1]
             , "numeric" = { do.call( DescNumNum, args=append( list(x=grp, y=x, xname=pred, yname=resp), dotargs.numeric.numeric )) }                        
             , "integer" = { do.call( DescNumNum, args=append( list(x=grp, y=x, xname=pred, yname=resp), dotargs.numeric.numeric )) }                        
             , "factor"  = { do.call( DescNumFact, args=append( list(x=x, grp=grp), dotargs.numeric.factor ) ) }                        
             , "ordered"  = { do.call( DescNumFact, args=append( list(x=x, grp=grp), dotargs.numeric.factor ) ) }                        
             , cat(gettextf("Don't know how to describe class: %s ~ %s!\n", class(x), class(grp)), "\n")

          )
        , "factor" =  switch( class(grp)[1]
              , "numeric" = { do.call( DescNumFact, args=append( list(x=grp, grp=x), dotargs.numeric.factor )) }
              , "integer" = { do.call( DescNumFact, args=append( list(x=grp, grp=x), dotargs.numeric.factor )) }
              , "factor"  = { do.call( DescFactFact, args=append( list(x=x, grp=grp, xname=resp, grpname=pred), dotargs.factor.factor) ) }
              , "ordered"  = { do.call( DescFactFact, args=append( list(x=x, grp=grp, xname=resp, grpname=pred), dotargs.factor.factor) ) }
              , cat(gettextf("Don't know how to describe class: %s ~ %s!\n", class(x), class(grp)), "\n")

          )
        , cat(gettextf("Don't know how to describe class: %s ~ %s!\n", class(x), class(grp)), "\n")
      )                
    }
  }  

}
