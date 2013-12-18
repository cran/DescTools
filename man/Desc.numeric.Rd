\name{Desc.numeric}
\alias{Desc.numeric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Describe a numeric vector
%%  ~~function to do ... ~~
}
\description{This reports a rich description of a numeric vector, consisting of the most common descriptive measures for location
and variability.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{Desc}{numeric}(x, xname = NULL, highlow = TRUE, plotit = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a single numeric vector to be described.
%%     ~~Describe \code{x} here~~
}
  \item{xname}{the caption for the output.
%%     ~~Describe \code{xname} here~~
}
   \item{highlow}{boolean. Should the highest and the lowest values be reported. This is usually a good idea and so the default is \code{TRUE}.
%%     ~~Describe \code{highlow} here~~
}
  \item{plotit}{boolean. Should a plot be created? The vector would be plotted by means of \code{\link{PlotDesc.numeric}}, which again basically is 
  \code{\link{PlotFdist}}. Default is \code{FALSE}.
%%     ~~Describe \code{plotit} here~~
}
  \item{\dots}{further argument to be passed to methods.}
}
\details{The plot function used here is \code{\link{PlotFdist}}.
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
  \item{meanSE}{standard error of the mean, as calculated by \code{\link{MeanSE}}.} 
  \item{quant}{a table of quantiles, as calculated by 
    \code{\link{quantile}} with probs set to c(.05, .10, .25, .5, .75, .9, .95).} 
  \item{sd}{standard deviation} 
  \item{vcoef}{coefficient of variation: mean(x) / sd(x) } 
  \item{mad}{median absolute deviation (\code{\link{mad}}) } 
  \item{IQR}{interquartile range } 
  \item{skew}{skewness, as calculated by \code{\link{Skew}}. } 
  \item{kurt}{kurtosis, as calculated by \code{\link{Kurt}}.} 
  \item{highlow}{the lowest and the highest values, reported with their frequencies in brackets, if > 1.} 

}


\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{Desc}}, \code{\link{PlotDesc}}
}
\examples{

Desc(d.pizza$temperature)

# contains all results of:
quantile(d.pizza$temperature, na.rm=TRUE)

}
\keyword{ univar }


