\name{PoissonCI}
\alias{PoissonCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Poisson Confidence Interval
%%  ~~function to do ... ~~
}
\description{Computes the confidence intervals of a poisson distributed variable's lambda. Several methods are implemented, see details.
}
\usage{
PoissonCI(x, n = 1, conf.level = 0.95, method = c("exact", "score", "wald"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{number of events. }
  \item{n}{time base for event count.}
 \item{conf.level}{confidence level, defaults to 0.95.}
 \item{method}{ character string specifing which method to use; can be one out of 
  \code{"wald"}, \code{"score"}.
  Method can be abbreviated. See details. Defaults to \code{"score"}. }

}
\details{
  The Wald interval uses the asymptotic normality of the test statistic. 

}
\value{
  A vector with 3 elements for estimate, lower confidence intervall and upper for the upper one.  
}
\references{  
Agresti, A. and Coull, B.A. (1998) Approximate is better than "exact" for interval 
estimation of binomial proportions. \emph{American Statistician}, \bold{52}, pp. 119-126.  

Garwood, F. (1936) Fiducial Limits for the Poisson distribution. \emph{Biometrika} 28:437-442.

\url{http://www.ine.pt/revstat/pdf/rs120203.pdf}

}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{poisson.test}}, \code{\link{BinomCI}}, \code{\link{MultinomCI}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# the horse kick example
count <- 0:4
deaths <- c(144, 91, 32, 11, 2)

n <- sum(deaths)
x <- sum(count * deaths)

lambda <- x/n

PoissonCI(x=x, n=n, method = c("exact","score", "wald"))

exp <- dpois(0:4, lambda) * n

barplot(rbind(deaths, exp * n/sum(exp)), names=0:4, beside=TRUE, 
  col=c(hred, hblue), main = "Deaths from Horse Kicks", xlab = "count")
legend("topright", legend=c("observed","expected"), fill=c(hred, hblue), 
  bg="white")


## SMR, Welsh Nickel workers
PoissonCI(x=137, n=24.19893)
}
\keyword{ univar }

