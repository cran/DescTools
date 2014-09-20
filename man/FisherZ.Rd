\name{FisherZ}
\alias{FisherZ}
\alias{FisherZInv}
\alias{CorCI}


\title{ Fisher r to z and z to r and confidence intervals}
\description{Convert a correlation to a z score or z to r using the Fisher transformation or find the confidence intervals for a specified correlation.  
}
\usage{
FisherZ(rho)
FisherZInv(z)
CorCI(rho, n, conf.level = 0.95, twotailed = TRUE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rho}{ a Pearson r }
  \item{z}{A Fisher z}
  \item{n}{Sample size for confidence intervals}
  \item{conf.level}{Confidence interval}
  \item{twotailed}{Treat p as twotailed p}
}

\value{ z value corresponding to r  (FisherZ) \\
r corresponding to z (FisherZInv) \\
rho, lower and upper confidence intervals (CorCI) \\
}


\author{William Revelle <revelle@northwestern.edu>
}

\examples{
cors <- seq(-.9,.9,.1)

zs <- FisherZ(cors)
rs <- FisherZInv(zs)
round(zs, 2)
n <- 30
r <- seq(0,.9,.1)
rc <- t(sapply(r, CorCI, n=n))
t <- r*sqrt(n-2)/sqrt(1-r^2)
p <- (1-pt(t,n-2))/2

r.rc <- data.frame(r=r, z=FisherZ(r), lower=rc[,2], upper=rc[,3], t=t, p=p)

round(r.rc,2)
}

\keyword{ multivariate }% at least one, from doc/KEYWORDS
\keyword{ models }% __ONLY ONE__ keyword per line
