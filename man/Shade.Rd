\name{Shade}
\alias{Shade}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Produce a Shaded Curve
%%  ~~function to do ... ~~
}
\description{
Sometimes the area under a density on a graphic has to be color shaded, for instance to illustrate a p-value or a region under the normal curve.
This function offers a function interface for doing this with less code.
}
\usage{
Shade(FUN, col = par("fg"), breaks, density = 10, step = 0.01, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FUN}{the function to be evaluated, best given as text: \code{FUN="f(x)"}.
%%     ~~Describe \code{FUN} here~~
}
  \item{col}{color to fill or shade the shape with. The default is taken from \code{par("fg")}.
%%     ~~Describe \code{col} here~~
}
  \item{breaks}{numeric, a vector giving the breakpoints between the distinct areas to be shaded differently. Should be finite as there are no plots with infinite limits.
%%     ~~Describe \code{xlim} here~~
}
  \item{density}{the density of the lines as needed in polygon.
%%     ~~Describe \code{density} here~~
}
  \item{step}{the step for the x-values to be used for calculating the function values.
%%     ~~Describe \code{step} here~~
}
  \item{\dots}{the dots are passed on to \code{\link{polygon}}.
%%     ~~Describe \code{\dots} here~~
}
}
\details{Useful for shading the area under a curve as often needed for explaining significance tests.
%%  ~~ If necessary, more details than the description above ~~
}
\value{nothing returned
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{ \code{\link{polygon}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
curve(dt(x, df=5), xlim=c(-6,6),
      main=paste("Student t-Distribution Probability Density Function, df = ", 5, ")", sep=""),
      type="n", las=1, ylab="probability", xlab="t")

Shade(FUN="dt(x, df=5)", breaks=c(-6, qt(0.025, df=5), qt(0.975, df=5), 6),
      col=c(hred, hblue), density=c(20, 7))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot }% use one of  RShowDoc("KEYWORDS")
