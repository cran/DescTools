\name{PlotACF}
\alias{PlotACF}
\alias{PlotGACF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Combined Plot of a Time Series and it's Autocorrelation and Partial Autocorrelation
%%  ~~function to do ... ~~
}
\description{Combined Plot of a Time Series and it's Autocorrelation and Partial Autocorrelation
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotACF(series, lag.max = 10 * log10(length(series)), ...)
PlotGACF(series, lag.max=10*log10(length(series)), type="corr", ylab="Auto-Korr.", ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{series}{univariate time series.
%%     ~~Describe \code{series} here~~
}
  \item{lag.max}{integer. Defines the number of lags to be displayed. The default
  is 10 * log10(length(series)). 
%%     ~~Describe \code{lag.max} here~~
}
  \item{type}{character string giving the type of acf to be computed. Allowed values are \code{"correlation"} (the default), \code{"covariance"} or \code{"partial"}.
%%     ~~Describe \code{lag.max} here~~
}
  \item{ylab}{a title for the y axis: see \code{\link{title}}.
%%     ~~Describe \code{lag.max} here~~
}
  \item{\dots}{the dots are passed to the plot command.
%%     ~~Describe \code{\dots} here~~
}
}
\details{PlotACF plots a combination of the time series and its autocorrelation and partial autocorrelation. PlotGACF is used as 
subfunction to produce the acf- and pacf-plots.
%%  ~~ If necessary, more details than the description above ~~
}
\author{Markus Huerzeler, ETH Zurich
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{ts}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
PlotACF(AirPassengers)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
