ZTest.default <-
function (x, y = NULL, alternative = c("two.sided", "less", "greater"), 
                           paired = FALSE, mu = 0, sd_pop, conf.level = 0.95,  ...)  {
  
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
  
  if (paired) {
    x <- x - y
    y <- NULL
  }
  
  nx <- length(x)
  mx <- mean(x)
  # vx <- sd_pop^2
  
  if (is.null(y)) {
    if (nx < 2) 
      stop("not enough 'x' observations")
    stderr <- sqrt(sd_pop^2/nx)
    if (stderr < 10 * .Machine$double.eps * abs(mx)) 
      stop("data are essentially constant")
    zstat <- (mx - mu)/stderr
    
    method <- method <- if (paired) 
      "Paired z-test" else "One Sample z-test"
    estimate <- setNames(mx, if (paired) 
      "mean of the differences"
      else "mean of x")
  }
  else {
    ny <- length(y)
    if (nx < 1) 
      stop("not enough 'x' observations")
    if (ny < 1) 
      stop("not enough 'y' observations")
    if (nx + ny < 3) 
      stop("not enough observations")
    my <- mean(y)

    method <- paste("Two Sample z-test")
    estimate <- c(mx, my)
    names(estimate) <- c("mean of x", "mean of y")
    
    stderr <- sqrt(sd_pop^2 * (1/nx + 1/ny))

    if (stderr < 10 * .Machine$double.eps * max(abs(mx), 
                                                abs(my))) 
      stop("data are essentially constant")
    zstat <- (mx - my - mu)/stderr
  }
  if (alternative == "less") {
    pval <- pnorm(zstat)
    cint <- c(-Inf, zstat + qnorm(conf.level))
  }
  else if (alternative == "greater") {
    pval <- pnorm(zstat, lower.tail = FALSE)
    cint <- c(zstat - qnorm(conf.level), Inf)
  }
  else {
    pval <- 2 * pnorm(-abs(zstat))
    alpha <- 1 - conf.level
    cint <- qnorm(1 - alpha/2)
    cint <- zstat + c(-cint, cint)
  }
  cint <- mu + cint * stderr
  names(zstat) <- "z"
  names(mu) <- if (paired || !is.null(y)) 
    "difference in means"
  else "mean"
  names(sd_pop) <- "Std. Dev. Population"
  attr(cint, "conf.level") <- conf.level
  rval <- list(statistic = zstat, p.value = pval, 
               parameter = sd_pop,
               conf.int = cint, estimate = estimate, null.value = mu, 
               alternative = alternative, method = method, data.name = dname)
  class(rval) <- "htest"
  return(rval)
}
