\name{Desc.integer}
\alias{Desc.integer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Describe an integer variable
%%  ~~function to do ... ~~
}
\description{
Describing an integer, means typically count data, is sometimes the same as describing an ordered factor, and somethimes,
when there are many levels, it is like describing a numeric value. 
}
\usage{
\method{Desc}{integer}(x, xname = NULL, maxlevels = 12, 
             digits = 3, plotit = FALSE, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a single integer vector to be described.
%%     ~~Describe \code{x} here~~
}
  \item{xname}{the caption for the output.
%%     ~~Describe \code{xname} here~~
}
  \item{maxlevels}{integer. Until how many levels should the detailed frequencies be reported. Default is 12. 
  If there are more levels, then the output will fallback to the presentation of highest and lowest values as used in
  for the representation of numeric variables.
  Set maxlevels to NA, if no restriction is to be applied, say the frequency table should contain all existing levels.
%%     ~~Describe \code{maxlevels} here~~
}
  \item{digits}{integer. With how many digits shoud the relative frequencies be formatted? Default is 3.
%%     ~~Describe \code{digits} here~~
}
  \item{plotit}{boolean. Should a plot be created? The vector would be plotted by means of \code{\link{PlotDesc.numeric}}, which again basically is 
  \code{\link{PlotFdist}}. Default is FALSE.
%%     ~~Describe \code{plotit} here~~
}
  \item{\dots}{further argument to be passed to methods.}
}
\details{A horizontal barplot would be suitable as well here.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list containing the following components: 
  \item{length}{the length of the vector (n + NAs).}
  \item{n}{the valid entries (NAs are excluded)}
  \item{NAs}{number of NAs}
  \item{unique}{number of unique values. } 
  \item{0s}{number of zeros} 
  \item{mean}{arithmetic mean} 
  \item{MeanSE}{standard error of the mean, as calculated by \code{\link{MeanSE}}.} 
  \item{quant}{a table of quantiles, as calculated by 
  \code{\link{quantile}(x, probs = c(.05,.10,.25,.5,.75,.9,.95), na.rm = TRUE)}.} 
  \item{sd}{standard deviation} 
  \item{vcoef}{coefficient of variation: \code{mean(x)} / \code{sd(x)} } 
  \item{mad}{median absolute deviation (\code{\link{mad}}) } 
  \item{IQR}{interquartile range } 
  \item{skew}{skewness, as calculated by \code{\link{Skew}}. } 
  \item{kurt}{kurtosis, as calculated by \code{\link{Kurt}}.} 
  \item{highlow}{the lowest and the highest values, reported with their frequencies in brackets, if > 1.} 
  \item{frq}{a data.frame of absolute and relative frequencies given by \code{\link{Freq}} if maxlevels > unique values in the vector.} 
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{Desc.factor}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

# default
Desc(d.pizza$count)

# with frequency table
Desc(d.pizza$count, maxlevels=15)

# Result object
res <- Desc(d.pizza$count, maxlevels=15)
res

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }
