ParseFormula <-
function(formula, data=parent.frame(), drop = TRUE) {
  
  xhs <- function(formula, data = parent.frame(), na.action=na.pass){
    
    # get all variables out of the formula
    vars <- attr(terms(formula, data=data), "term.labels")
    
    # evaluate model.frame
    mf <- match.call(expand.dots = FALSE) 
    m <- match(c("formula", "data", "na.action"), names(mf), 0) 
    mf <- mf[c(1, m)] 
    mf$na.action <- na.action
    mf$drop.unused.levels <- TRUE 
    mf[[1]] <- as.name("model.frame") 
    
    mf.rhs <- eval.parent(mf) 
    
    # model frame does not evaluate interaction, so let's do that here
    d.tmp <- mf.rhs[,FALSE] # create a new data.frame
    for(x in vars){
      if( length(grep(":", x))>0 )      # there's a : in the variable
        d.tmp <- data.frame(d.tmp, 
           interaction( mf.rhs[, names(mf.rhs)[names(mf.rhs) %in% unlist(strsplit(x, ":"))]], 
              sep=":", drop = drop)      # set drop unused levels to TRUE here by default
        )
      else 
        d.tmp <- data.frame(d.tmp, mf.rhs[,x])  
    }
    names(d.tmp) <- vars

    return(list(formula=formula, mf=mf.rhs, mf.eval=d.tmp, vars=vars))
  }

  f1 <- formula

  # evaluate subset
  m <- match.call(expand.dots = FALSE)

  # do not support . on both sides of the formula
  if( (length(grep("^\\.$", all.vars(f1[[2]])))>0) && (length(grep("^\\.$", all.vars(f1[[3]])))>0) )
    stop("dot argument on both sides of the formula are not supported")
    
  # swap left and right hand side and take just the right side
  # so both sides are evaluated with right side logic, but independently
  lhs <- xhs(formula(paste("~", deparse(f1[[2]])), data=data), data=data) 
  rhs <- xhs(formula(paste("~", deparse(f1[[3]])), data=data), data=data) 

  # now handle the dot argument
  if(any(all.vars(f1[[2]]) == ".")){   # dot on the left side
    lhs$vars <- lhs$vars[!lhs$vars %in% rhs$vars]
    lhs$mf <- lhs$mf[lhs$vars]
    lhs$mf.eval <- lhs$mf.eval[lhs$vars]
  } else if(any(all.vars(f1[[3]]) == ".")){     # dot on the right side
    rhs$vars <- rhs$vars[!rhs$vars %in% lhs$vars]
    rhs$mf <- rhs$mf[rhs$vars]
    rhs$mf.eval <- rhs$mf.eval[rhs$vars]
  } else {    # no dot: do nothing
  }
  
  list(formula=formula, lhs=list(mf=lhs$mf, mf.eval=lhs$mf.eval, vars=lhs$vars), 
    rhs=list(mf=rhs$mf, mf.eval=rhs$mf.eval, vars=rhs$vars))
    
}
