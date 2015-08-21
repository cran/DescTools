\name{Desc.Date}
\alias{Desc.Date}

\title{Describe a Date Vector }

\description{Description interface for dates. We do here what seems reasonable for describing dates.
We start with a short summary about length, number of NAs and extreme values, before we describe the
frequencies of the weekdays and months, rounded up by a chi-square test.
}

\usage{
\method{Desc}{Date}(x, main = NULL, maxrows = 10, digits = NULL, dprobs = NULL, mprobs = NULL,
     plotit = getOption("plotit", FALSE), ...)

}

\arguments{
  \item{x}{the Date vector to be described.
}
  \item{main}{the caption of the output.
}
  \item{maxrows}{numeric. Defines the maximum number of rows to be reported. Default is 10 (most
  frequent ones).
  If maxrows < 1 then just as many rows, as the maxrows\% most frequent factors are shown.
  Say if maxrows is set to 0.8 then as many rows are shown, that the highest cumulative relative frequency is the first going beyond 0.8.
}
  \item{digits}{integer. With how many digits shoud the relative frequencies be formatted? Default is 1.
}
\item{dprobs}{a vector with the probabilities for the chisq.test of the days. If this is left to \code{NULL} (default) then a uniform distribution
will be used.}
\item{mprobs}{a vector with the probabilities for the chisq.test of the months. If this is left to \code{NULL} (default) then the distribution of days will be used (p = c(31/365, 28/365, 31/365, ...).}
  \item{plotit}{boolean. Should a plot be created? The factor is plotted with the factor interface of \code{\link{PlotDesc}}. Default is FALSE.

}
  \item{\dots}{further argument to be passed to methods.}

}

\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{Desc}}, \code{\link{PlotDesc}}
}
\examples{
Desc(d.pizza$date)
}
\keyword{ univar}
