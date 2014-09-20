HotellingsT2Test.default <-
function(X, Y=NULL, mu=NULL, test="f", na.action=na.fail, ...) {
    
    .HotellingsT  <-  function(X, Y=NULL, mu, test) {
      n <- dim(X)[1]
      p <- dim(X)[2]
      
      if(is.null(Y))     #one sample case
      {
        test.statistic <- n*as.numeric(t(colMeans(X)-mu)%*%solve(cov(X))%*%(colMeans(X)-mu))*switch(test,f=(n-p)/(p*(n-1)),chi=1)
        df.1 <- p
        df.2 <- switch(test,f=n-p,chi=NA) 
        p.value <- 1-switch(test,f=pf(test.statistic,df.1,df.2),chi=pchisq(test.statistic,df.1))
        return(list(test.statistic=test.statistic,p.value=p.value,df.1=df.1,df.2=df.2))
      }
      
      # else two sample case
      n1 <- n
      n2 <- dim(Y)[1]
      Xmeans <- colMeans(X)
      Ymeans <- colMeans(Y)
      X.diff <- sweep(X,2,Xmeans)
      Y.diff <- sweep(Y,2,Ymeans)
      S.pooled <- 1/(n1+n2-2)*(t(X.diff)%*%X.diff+t(Y.diff)%*%Y.diff)
      test.statistic <- n1*n2/(n1+n2)*t(Xmeans-Ymeans-mu)%*%solve(S.pooled)%*%(Xmeans-Ymeans-mu)*switch(test,f=(n1+n2-p-1)/(p*(n1+n2-2)),chi=1)
      df.1 <- p
      df.2 <- switch(test,f=n1+n2-p-1,chi=NA)
      p.value <- 1-switch(test,f=pf(test.statistic,df.1,df.2),chi=pchisq(test.statistic,df.1))
      list(test.statistic=test.statistic,p.value=p.value,df.1=df.1,df.2=df.2)
    }
    
    
    if (is.null(Y)) 
    {
      DNAME <- deparse(substitute(X))
    }
    else
    {
      DNAME=paste(deparse(substitute(X)),"and",deparse(substitute(Y)))
    }
    
    X <- na.action(X)
    if(!all(sapply(X, is.numeric))) stop("'X' must be numeric")
    X <- as.matrix(X)
    
    p <- dim(X)[2]
    
    if (!is.null(Y))
    {
      Y <- na.action(Y)
      if(!all(sapply(Y, is.numeric))) stop("'Y' must be numeric")
      if (p!=dim(Y)[2]) stop("'X' and 'Y' must have the same number of columns")
      Y <- as.matrix(Y)
    }
    
    if (is.null(mu)) mu <- rep(0,p) 
    else if (length(mu)!=p) stop("length of 'mu' must equal the number of columns of 'X'")
    
    test <- match.arg(test,c("f","chi"))
    
    if (is.null(Y) & test=="f") version <- "one.sample.f"
    if (is.null(Y) & test=="chi") version <- "one.sample.chi"
    if (!is.null(Y) & test=="f") version <- "two.sample.f"
    if (!is.null(Y) & test=="chi") version <- "two.sample.chi"
    
    res1 <- switch(version,
                 "one.sample.f"={
                   result <- .HotellingsT(X,mu=mu,test=test)
                   STATISTIC <- result$test.statistic
                   names(STATISTIC) <- "T.2"
                   PVAL <- result$p.value
                   METHOD <- "Hotelling's one sample T2-test"
                   PARAMETER <- c(result$df.1,result$df.2)
                   names(PARAMETER) <- c("df1","df2")
                   RVAL <- list(statistic=STATISTIC,p.value=PVAL,method=METHOD,parameter=PARAMETER)
                   
                   RVAL}
                 ,
                 "one.sample.chi"={
                   result <- .HotellingsT(X,mu=mu,test=test)
                   STATISTIC <- result$test.statistic
                   names(STATISTIC) <- "T.2"
                   PVAL <- result$p.value
                   METHOD <- "Hotelling's one sample T2-test"
                   PARAMETER <- c(result$df.1)
                   names(PARAMETER) <- c("df")
                   RVAL <- list(statistic=STATISTIC,p.value=PVAL,method=METHOD,parameter=PARAMETER)
                   
                   RVAL}
                 ,
                 "two.sample.f"={
                   result <- .HotellingsT(X,Y,mu,test)
                   STATISTIC <- result$test.statistic
                   names(STATISTIC) <- "T.2"
                   PVAL <- result$p.value
                   METHOD <- "Hotelling's two sample T2-test"
                   PARAMETER <- c(result$df.1,result$df.2)
                   names(PARAMETER) <- c("df1","df2")
                   RVAL <- list(statistic=STATISTIC,p.value=PVAL,method=METHOD,parameter=PARAMETER)
                   
                   RVAL}
                 ,
                 "two.sample.chi"={
                   result <- .HotellingsT(X,Y,mu,test)
                   STATISTIC <- result$test.statistic
                   names(STATISTIC) <- "T.2"
                   PVAL <- result$p.value
                   METHOD <- "Hotelling's two sample T2-test"
                   PARAMETER <- c(result$df.1)
                   names(PARAMETER) <- c("df")
                   RVAL <- list(statistic=STATISTIC,p.value=PVAL,method=METHOD,parameter=PARAMETER)
                   
                   RVAL}
    )
    ALTERNATIVE="two.sided"
    NVAL <- paste("c(",paste(mu,collapse=","),")",sep="")
    if (is.null(Y)) names(NVAL) <- "location" else names(NVAL) <- "location difference"
    res <- c(res1,list(data.name=DNAME,alternative=ALTERNATIVE,null.value=NVAL))
    class(res) <- "htest"
    return(res)
  }
