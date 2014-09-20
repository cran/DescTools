Desc.table <-
function(x, main=NULL, rfrq = NULL, margins = c(1,2), 
                       plotit=getOption("plotit", FALSE), verbose = c("medium","low","high"), ... ){

  if(length(dim(x)) > 2) Desc.default(x)  # no more than 2 dimensions for a table
  
  # define verbosity
  verbose <- match.arg(verbose, c("medium","low","high"))
  verbose <- match(verbose, c("low","medium","high"), nomatch=2)
  
  if(is.null(rfrq)) rfrq <- ifelse(verbose > 1, "111","000")
  
  if(is.null(main)) main <- gettextf("%s (%s)", deparse(substitute(x)), paste(class(x), collapse=", "))
  
  if(!identical(main, NA)) {
    cat( paste(rep("-",(as.numeric(options("width"))-2)), collapse=""), "\n" ) 
    cat(main, "\n")
  }  
  if( !is.null(attr(x,"label")) ) cat(" :", strwrap(attr(x,"label"), indent=2, exdent=2), sep="\n" )

  # Pairs summary
  n <- sum(x)
#  vn <- sum(complete.cases(x,grp))
#  digits <- format.info(signif((n-vn)/n*100,3))[2]-2    ### hier 3 signifikante Stellen für beide Angaben bestimmen
  if(length(dim(x))==1) {       # 1-dim table ****
    cat("\nSummary: \n",
        "n: ", n,
        ", rows: ", dim(x)[1]  
        , "\n\n", sep="" ) 
    
    r.chisq <- chisq.test(x)
    cat("Pearson's Chi-squared test:\n  "
        , capture.output(r.chisq)[5], "\n\n", sep="")
    
  } else {                   # n-dim tabl *****

      cat("\nSummary: \n",
      "n: ", n,
      ", rows: ", dim(x)[1],  
      ", columns: ", dim(x)[2]  
  	  , "\n\n", sep="" ) 
  
    if(dim(x)[1] == 2 & dim(x)[2] == 2 ){
      cat("Fisher's exact test ", capture.output( fisher.test(x))[5], "\n", sep="")
      if(verbose > 1){ # print only with verbosity > 1
        cat("", capture.output( mcnemar.test(x))[5], "\n\n", sep="")
        m <- ftable(format(rbind(
           "odds ratio    " = OddsRatio(x, conf.level=0.95)
          , "rel. risk (col1)  " = RelRisk(x, conf.level=0.95, method="wald", delta=0)
          , "rel. risk (col2)  " = RelRisk(x[,c(2,1)], conf.level=0.95, method="wald", delta=0)
        ), digits=3, nsmall=3))
                    
        attr(m, "col.vars")[[1]][1] <- "estimate"
        print(m)
      }
      
    } else {
      r.chisq <- chisq.test(x)
      cat("Pearson's Chi-squared test:\n  "
        , capture.output(r.chisq)[5], "\n", sep="")
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
          print(Assocs(x))
          cat("\n")
        }
    )       
  }

  print(PercTable(x, rfrq=rfrq, margins=margins, ...))
  cat("\n")
  
  if(plotit) PlotDesc.table(x, main=main)
  invisible()

}
