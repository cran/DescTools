\name{CoefVar}
\alias{CoefVar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Coefficient of Variation
%%  ~~function to do ... ~~
}
\description{Calculates the coefficient of variation and its confidence limits using the noncentral \code{t}-distribution..
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CoefVar(x, unbiased = FALSE, conf.level = NA, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.
%%     ~~Describe \code{x} here~~
}
  \item{unbiased}{logical. In order for the coefficient of variation to be an unbiased estimate of the true population value, 
   calculated value is calculated as CV * ((1 - (1/(4*(n-1))) + (1/n) * CV^2) + (1/(2 * (n-1)^2))). Default is TRUE. %% Reference???
%%     ~~Describe \code{unbiased} here~~
}
  \item{conf.level}{confidence level of the interval.
%%     ~~Describe \code{conf.level} here~~
}
  \item{na.rm}{logical. Should missing values be removed? Defaults to FALSE.
%%     ~~Describe \code{na.rm} here~~
}
 
}
\value{if no confidence intervals are requested:  
the estimate as numeric value (without any name)\cr\cr
else a named numeric vector with 3 elements 
\item{est}{estimate}
\item{lwr.ci}{lower confidence interval}
\item{upr.ci}{upper confidence interval}
}

\references{
Johnson, B. L., Welch, B. L. (1940). Applications of the non-central t-distribution. \emph{Biometrika}, 31, 362--389.

Kelley, K. (2007). Sample size planning for the coefcient of variation from the accuracy in parameter estimation approach. \emph{Behavior Research Methods, 39} (4), 755-766

Kelley, K. (2007). Constructing confidence intervals for standardized effect sizes: Theory, application, and implementation. \emph{Journal of Statistical Software, 20} (8), 1-24

McKay, A. T. (1932). Distribution of the coefficient of variation and the extended 
\emph{t} distribution, \emph{Journal of the Royal Statistical Society}, \emph{95}, 695--698.

Smithson, M.J. (2003) \emph{Confidence Intervals, Quantitative Applications in the Social Sciences Series}, No. 140. Thousand Oaks, CA: Sage. pp. 39-41

}
\author{Andri Signorell <andri@signorell.net>, Michael Smithson <michael.smithson@anu.edu.au> (noncentral-t)
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{mean}}, \code{\link{sd}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
set.seed(15)
x <- runif(100)
CoefVar(x, conf.level=0.95)

#       est    low.ci    upr.ci 
# 0.5092566 0.4351644 0.6151409 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
