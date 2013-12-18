\name{PlotDotCIp}
\alias{PlotDotCIp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot a Dotchart with Binomial Confidence Intervals
%%  ~~function to do ... ~~
}
\description{Plot a dotchart with binomial confidence intervals ("wilson") as segments.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotDotCIp(x, n, xlim = c(0, 1), 
           ord = c("rel", "abs", "names"), decreasing = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{number of successes, or a vector of length 2 giving the numbers of successes and failures, respectively.

%%     ~~Describe \code{x} here~~
}
  \item{n}{number of trials; ignored if x has length 2.
%%     ~~Describe \code{n} here~~
}
  \item{xlim}{the x limits of the plot.
%%     ~~Describe \code{xlim} here~~
}
  \item{ord}{how should the result be ordered? Default is relative frequency \code{"rel"}. The argument can be abbreviated. 
      
}
  \item{decreasing}{logical, sort order decreasing or ascending?
%%     ~~Describe \code{decreasing} here~~
}
  \item{\dots}{further arguments are passed to the function \code{PlotDotCI()}.
%%     ~~Describe \code{\dots} here~~
}
}
%%\details{ no details so far
%%  ~~ If necessary, more details than the description above ~~
%%}


\author{
Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{PlotDotCI}} }

\examples{

tab <- table(d.pizza$driver, d.pizza$wine_delivered)

PlotDotCIp(x=tab[,2], n=apply(tab,1,sum), ord="abs", dec=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }

