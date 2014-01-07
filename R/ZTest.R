ZTest <-
function(x, mu = 0, sd_pop,
                   alternative = c("two.sided", "less", "greater"),
                   conf.level = 0.95 ){

  if(missing(sd_pop)) stop("You must specify a standard deviation of the population")

  alternative <- match.arg(alternative)

  n <- length(x)
  z <- (mean(x)-mu)/(sd_pop/sqrt(n))

  out <- list(statistic=c(z=z))
  class(out) <- 'htest'

  out$parameter <- c(n=n, "Std. Dev." = sd_pop,
                     "Std. Dev. of the sample mean" = sd_pop/sqrt(n))

  out$p.value <- switch(alternative,
                    two.sided = 2*pnorm(abs(z),lower.tail=FALSE),
                    less = pnorm(z),
                    greater = pnorm(z, lower.tail=FALSE) )

  out$conf.int <- switch(alternative,
                         two.sided = mean(x) +
                           c(-1,1)*qnorm(1-(1-conf.level)/2)*sd_pop/sqrt(n),
                         less = c(-Inf, mean(x)+qnorm(conf.level)*sd_pop/sqrt(n)),
                         greater = c(mean(x)-qnorm(conf.level)*sd_pop/sqrt(n), Inf)
                         )
  attr(out$conf.int, "conf.level") <- conf.level

  out$estimate <- c("mean of x" = mean(x))
  out$null.value <- c("mean" = mu)
  out$alternative <- alternative
  out$method <- "One Sample z-test"
  out$data.name <- deparse(substitute(x))
  names(out$estimate) <- paste("mean of", out$data.name)

  return(out)
  
#   structure(list(statistic = c(S = S),
#                  p.value = PVAL,
#                  parameter = c(n=n, "Std. Dev." = sd_pop,
#                               "Std. Dev. of the sample mean" = sd_pop/sqrt(n))  
#                  method = "One Sample z-test",
#                  alternative = "extreme values are more likely in x than in y",
#                  data.name = DNAME),
#             class = "htest")
  
}
