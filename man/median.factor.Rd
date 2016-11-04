\name{median.factor}
\alias{median.factor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Median for Ordered Factors
%%  ~~function to do ... ~~
}
\description{Calculate the median for ordered factors. This is not implemented in standard R, as 
it's not well defined (it is not clear what to do if the median sits between two levels in factors of even length).
This function returns the high median and prints a warning if the low median would be different (which is
supposed to be a rare event).
}
\usage{
\method{median}{factor}(x, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an ordered factor containing the values whose median is to be computed.
%%     ~~Describe \code{x} here~~
}
  \item{na.rm}{a logical value indicating whether \code{NA} values should be stripped before the computation proceeds.
%%     ~~Describe \code{na.rm} here~~
}
}
\details{There's a vivid discussion between experts going on whether this should be defined or not.
We'll wait for definitive results and enjoy the function's comfort so far...
%%  ~~ If necessary, more details than the description above ~~
}
\value{a level of the ordered factor.
}

\author{Andri Signorell <andri@signorell.net>, based on code from Hong Ooi
%%  ~~who you are~~
}

\seealso{
\url{https://stat.ethz.ch/pipermail/r-help/2003-November/042684.html}

\url{http://stackoverflow.com/questions/7925102/idiomatic-method-of-finding-the-median-of-an-ordinal}
}
\examples{
median(d.pizza$quality, na.rm=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }
