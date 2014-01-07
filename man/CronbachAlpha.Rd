\name{CronbachAlpha}
\alias{CronbachAlpha}
\title{Cronbach's Coefficient Alpha}
\description{
  Computes Cronbach's alpha. 
  Cronbach's alpha determines the internal consistency or average correlation of items in a survey instrument to gauge its reliability. 
  This reduces to KR-20 when the columns of the data matrix are dichotomous.
}
\usage{
CronbachAlpha(x, conf.level = NA, cond = FALSE)
}
\arguments{
  \item{x}{A data frame or matrix with item responses. }
  \item{conf.level}{confidence level of the interval. If set to \code{NA} (which is the default) no confidence interval will be calculated.}
  \item{cond}{logical. If set to \code{TRUE}, alpha is additionally calculated for the dataset with each item left out. }
}

\value{
Either a numeric value or \cr
a named vector of 3 columns if confidence levels are required (estimate, lower and upper ci) or \cr

a list containing the following components, if the argument \code{cond} is set to \code{TRUE}:
  \item{unconditional}{Cronbach's Alpha, either the single value only or with confidence intervals}
  \item{condCronbachAlpha}{The alpha that would be realized if the item were excluded}
}


\seealso{ \code{\link{CohenKappa}}, \code{\link{Kappam}}
}
\references{
Cohen, J. (1960), A coefficient of agreement for nominal scales. \emph{Educational and Psychological Measurement}, 20, 37-46. 
}


\author{Andri Signorell <andri@signorell.net>, based on code of Harold C. Doran}
\examples{

set.seed(1234)
tmp <- data.frame(
  item1 = sample(c(0,1), 20, replace=TRUE), 
  item2 = sample(c(0,1), 20, replace=TRUE),
  item3 = sample(c(0,1), 20, replace=TRUE),
  item4 = sample(c(0,1), 20, replace=TRUE),
  item5 = sample(c(0,1), 20, replace=TRUE)
  )

CronbachAlpha(tmp[,1:4], cond=FALSE, conf.level=0.95)
CronbachAlpha(tmp[,1:4], cond=TRUE, conf.level=0.95)

CronbachAlpha(tmp[,1:4], cond=FALSE)
CronbachAlpha(tmp[,1:2], cond=TRUE, conf.level=0.95)

\dontrun{ 
# Calculate bootstrap confidence intervals for CronbachAlpha
library(boot)
cronbach.boot <- function(data,x) {CronbachAlpha(data[x,])[[3]]}
res <- boot(datafile, cronbach.boot, 1000)
quantile(res$t, c(0.025,0.975))   # two-sided bootstrapped confidence interval of Cronbach's alpha
boot.ci(res, type="bca")          # adjusted bootstrap percentile (BCa) confidence interval (better)
}

}

\keyword{multivar}
