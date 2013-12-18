DescWrd.formula <-
function(formula, data = parent.frame(), wrd, ...) {

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

  # ### start output
  # cat("\nCall:\n")
  # cat(paste(deparse(sys.call()), sep = "\n", collapse = "\n"),"\n\n", sep = "")
  
  # start analysis
  for(resp in mm$lhs$vars){         # for all response variables
    for(pred in mm$rhs$vars){       # evalutate for all conditions
      x <- mm$lhs$mf.eval[,resp]
      grp <- mm$rhs$mf.eval[,pred]
  
     # Titel am Anfang
      WrdCaption( x=paste(resp, " ~ ", pred, sep=""), wrd=wrd )
      
       vclass <-  c(class(x)[1], class(grp)[1] )
      
      if( all(vclass == "factor" ) ) {                   # type: "factor.factor"  ***********

            WrdText( capture.output( 
              do.call(DescFactFact, args=append(list(x=x, grp=grp, xname=resp, grpname=pred), dotargs.factor.factor)) ) 
              , wrd=wrd) 
            PlotDesc(table(x, grp) , wrd=wrd, main="")

      } else if( all(vclass %in% c("numeric","integer") ) ) {
                                                          # type: "numeric.numeric" ******
            WrdText( capture.output(
              do.call(DescNumNum, args=append( list(x=grp, y=x, xname=pred, yname=resp), dotargs.numeric.numeric))
              ), wrd=wrd)

            # Scatterplot für numeric ~ numeric Darstellung
            PlotDescNumNum( form1=formula(paste(resp, "~", pred))
              , form2=formula(paste(pred, "~", resp)), data=data, wrd=wrd, main="" )

      } else if( any(vclass %in% c("factor","ordered") ) & 
                any(vclass %in% c("numeric","integer") ) ){
                                                         # type: "numeric.factor" ********
            if(class(x)[1] %in% c("factor","ordered")) {
               # swap target and grouping variable
               warning(gettextf("conditional and response variables were swapped: %s ~ %s instead of %s ~ %s", pred, resp, resp, pred) )
               tmp <- x; x <- grp; grp <- tmp
               tmp <- resp; resp <- pred; pred <- tmp
            } 
            
            WrdText( capture.output(
              do.call(DescNumFact, args=append(list(x=x, grp=grp), dotargs.numeric.factor))
              ), wrd=wrd)
            PlotDescNumFact(formula=formula(paste(resp, "~", pred)), data=data, wrd=wrd, main="")
                                
      } else {
        # atype <- NULL  
        cat(gettextf("Don't know how to describe classes: %s | %s \n\n", class(x), class(grp)) ) 
      }

    }
  }  

}
