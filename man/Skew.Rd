\name{Measures of Shape}
\alias{Skew}
%%\alias{Skew.default}
%%\alias{Skew.matrix}
%%\alias{Skew.data.frame}
\alias{Kurt}
%%\alias{Kurt.default}
%%\alias{Kurt.matrix}
%%\alias{Kurt.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Skewness and Kurtosis
%%  ~~function to do ... ~~
}
\description{
\code{Skew} computes the skewness, \code{Kurt} the kurtosis of the values in x. 

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{

Skew(x, na.rm = FALSE, method = 3, conf.level = NA, type = "bca", R = 1000)

Kurt(x, na.rm = FALSE, method = 3, conf.level = NA, type = "bca", R = 1000)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  a numeric vector, matrix or data frame. An object which is not a vector, matrix or data frame is coerced (if possible) by as.vector.
%%     ~~Describe \code{x} here~~
}
  \item{na.rm}{
  logical, indicating whether NA values should be stripped before the computation proceeds. Defaults to FALSE.
  %%     ~~Describe \code{na.rm} here~~
}
  \item{method}{
  integer out of 1, 2 or 3. Default ist 3. See Details.
  %%     ~~Describe \code{na.rm} here~~
}
 \item{conf.level}{confidence level of the interval. If set to NA (which is the default) no confidence interval will be calculated. 
%%     ~~Describe \code{conf.level} here~~
}
  \item{type}{The type of confidence interval required. The value should be any subset 
 of the values \code{"norm"}, \code{"basic"}, \code{"stud"}, \code{"perc"}, \code{"bca"}) or simply \code{"all"} 
 which will compute all five types of intervals. 
}
\item{R}{The number of bootstrap replicates. Usually this will be a single positive integer. For importance resampling, 
 some resamples may use one set of weights and others use a different set of weights. In this case \code{R} would be a vector 
 of integers where each component gives the number of resamples from each of the rows of weights.
}
}

\details{
If x is a matrix or a data frame, a vector of the skewness, resp. kurtosis, of the columns is returned. \cr

If na.rm is TRUE then missing values are removed before computation proceeds. \cr

The type of skewness can either be:\cr
  \code{type = 1:   g_1 = m_3 / m_2^(3/2) } \cr
  \code{type = 2:   G_1 = g_1 * sqrt(n(n-1)) / (n-2) }\cr
  \code{type = 3:   b_1 = m_3 / s^3 = g_1 ((n-1)/n)^(3/2) } \cr

The type of kurtosis can either be:\cr
  \code{type = 1:  g_2 = m_4 / m_2^2 - 3 }  \cr
  \code{type = 2:  G_2 = ((n+1) g_2 + 6) * (n-1) / ((n-2)(n-3)) }  \cr
  \code{type = 3:  b_2 = m_4 / s^4 - 3 = (g_2 + 3) (1 - 1/n)^2 - 3 } \cr

  type = 1 is the typical definition used in many older textbooks.   \cr
  type = 2 is used in SAS and SPSS.  \cr
  type = 3 is used in MINITAB and BMDP. \cr

Skew and Kurtosis are comparably fast, as the expensive sums are coded in C.

}
\note{
Cramer et al. (1997) mention the asymptotic standard error of the skewness \cr
\preformatted{ASE.skew = sqrt( 6n(n-1)/((n-2)(n+1)(n+3)) ), resp. 
ASE.kurt = sqrt( (n^2 - 1)/((n-3)(n+5)) )} for the kurtosis, to be used for calculating the confidence intervals.\cr
Joanes and Gill advise against that, pointing out that the normal assumptions would virtually always be violated. 
They suggest using the bootstrap method.\cr  
}

\value{For a data frame or for a matrix, a named vector with the appropriate method being applied column by column. 
}
\author{Andri Signorell <andri@signorell.net>, David Meyer <david.meyer@r-project.org> (type = 3) }

\references{
Cramer, D. (1997): \emph{Basic Statistics for Social Research} Routledge.

Joanes, D. N., Gill, C. A. (1998): Comparing measures of sample skewness and Kurt. \emph{The Statistician}, 47, 183-189.\cr

}

\seealso{
\code{\link{mean}}, \code{\link{sd}}, similar code in \code{library(e1071)}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Skew(d.pizza$price, na.rm=TRUE)
Kurt(d.pizza$price, na.rm=TRUE)

# use sapply to calculate skewness for a data.frame
sapply(d.pizza[,c("temperature","price","delivery_min")], Skew, na.rm=TRUE)

# or apply to do that columnwise with a matrix
apply(as.matrix(d.pizza[,c("temperature","price","delivery_min")]), 2, Skew, na.rm=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math}
