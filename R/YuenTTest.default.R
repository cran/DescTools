YuenTTest.default <-
function (x, y = NULL, alternative = c("two.sided", "less", "greater"), 
          mu = 0, paired = FALSE, conf.level = 0.95, trim = 0.2, ...) {
  
  alternative <- match.arg(alternative)
  if (!missing(mu) && (length(mu) != 1 || is.na(mu))) 
    stop("'mu' must be a single number")
  if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) || 
                                 conf.level < 0 || conf.level > 1)) 
    stop("'conf.level' must be a single number between 0 and 1")
  if (!is.null(y)) {
    dname <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    if (paired) 
      xok <- yok <- complete.cases(x, y)
    else {
      yok <- !is.na(y)
      xok <- !is.na(x)
    }
    y <- y[yok]
  }
  else {
    dname <- deparse(substitute(x))
    if (paired) 
      stop("'y' is missing for paired test")
    xok <- !is.na(x)
    yok <- NULL
  }
  x <- x[xok]

  nx <- length(x)
  mx <- mean(x, trim = trim)
  vx <- var(Winsorize(x, probs = c(trim, 1-trim)))  
  
  if (is.null(y) | paired) {
    if (nx < 2) 
      stop("not enough 'x' observations")
    
    df <- nx - 2 * floor(trim * nx) - 1
    
    if(paired){
      my <- mean(y, trim = trim)
      vy <- var(Winsorize(y, probs = c(trim, 1-trim)))  
      covxy <- var(Winsorize(x, probs = c(trim, 1-trim)), Winsorize(y, probs = c(trim, 1-trim))) 
      stderr <- sqrt( (nx-1) * (vx + vy - 2 * covxy) / ((df + 1) * df) )  
    } else {
      stderr <- sqrt(vx) / ((1 - 2 * trim) * sqrt(nx))   
    }

    if (stderr < 10 * .Machine$double.eps * abs(mx)) 
      stop("data are essentially constant")
    
    if(paired){
      method <- "Yuen Paired t-test"
      tstat <- (mx - my - mu) / stderr
      estimate <- setNames(mx - my, "difference of trimmed means")
        
    } else {
      method <- "Yuen One Sample t-test"
      tstat <- (mx - mu)/stderr
      estimate <- setNames(mx, "trimmed mean of x")
    }
    
  }
  else {
    ny <- length(y)
    if (nx < 2) 
      stop("not enough 'x' observations")
    if (ny < 2) 
      stop("not enough 'y' observations")
    my <- mean(y, trim = trim)
    vy <- var(Winsorize(y, probs = c(trim, 1-trim)))  
    method <- "Yuen Two Sample t-test"
    estimate <- c(mx, my)
    names(estimate) <- c("trimmed mean of x", "trimmed mean of y")

    dfx <- length(x) - 2 * floor(trim * length(x)) - 1
    dfy <- length(y) - 2 * floor(trim * length(y)) - 1
    
    stderrx <- (length(x) - 1) * vx / ((dfx + 1) * dfx)
    stderry <- (length(y) - 1) * vy / ((dfy + 1) * dfy)
    
    df <- (stderrx + stderry)^2 / (stderrx^2 / dfx + stderry^2 / dfy)
    
    stderr <- sqrt(stderrx + stderry)

    if (stderr < 10 * .Machine$double.eps * max(abs(mx), abs(my))) 
      stop("data are essentially constant")
    tstat <- (mx - my - mu) / stderr
  }
  if (alternative == "less") {
    pval <- pt(tstat, df)
    cint <- c(-Inf, tstat + qt(conf.level, df))
  }
  else if (alternative == "greater") {
    pval <- pt(tstat, df, lower.tail = FALSE)
    cint <- c(tstat - qt(conf.level, df), Inf)
  }
  else {
    pval <- 2 * pt(-abs(tstat), df)
    alpha <- 1 - conf.level
    cint <- qt(1 - alpha/2, df)
    cint <- tstat + c(-cint, cint)
  }
  cint <- mu + cint * stderr
  names(tstat) <- "t"
  names(df) <- "df"
  names(trim) <- "trim"
  names(mu) <- if (paired || !is.null(y)) 
    "difference in trimmed means"
  else "trimmed mean"
  attr(cint, "conf.level") <- conf.level
  rval <- list(statistic = tstat, parameter = c(df, trim), p.value = pval, 
               conf.int = cint, estimate = estimate, null.value = mu, 
               alternative = alternative, method = method, data.name = dname)
  class(rval) <- "htest"
  return(rval)
}
