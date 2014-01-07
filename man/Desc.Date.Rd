\name{Desc.Date}
\alias{Desc.Date}

\title{Describe a Date Vector }

\description{Description interface for dates. We do here what seems resonable for describing dates.
We start with a short summary about length, number of NAs and extreme values, before we describe the 
frequencies of the weekdays and months, rounded up by a chi-square test. 
}

\usage{
\method{Desc}{Date}(x, xname = NULL, maxrows = 10, 
          digits = 3, plotit = FALSE, ...)
      
}

\arguments{
  \item{x}{the Date vector to be described.
}
  \item{xname}{the caption of the output. 
}
  \item{maxrows}{numeric. Defines the maximum number of rows to be reported. Default is 10 (most
  frequent ones).
  If maxrows < 1 then just as many rows, as the maxrows\% most frequent factors are shown. 
  Say if maxrows is set to 0.8 then as many rows are shown, that the highest cumulative relative frequency is the first going beyond 0.8.
}
  \item{digits}{integer. With how many digits shoud the relative frequencies be formatted? Default is 3.
}
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
