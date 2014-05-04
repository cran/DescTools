\name{MoveAvg}
\alias{MoveAvg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Moving Average
%%  ~~function to do ... ~~
}
\description{Compute a simple moving average (running mean).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MoveAvg(x, order, align = c("center", "left", "right"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{univariate time series.
%%     ~~Describe \code{x} here~~
}
  \item{order}{order of moving average.
%%     ~~Describe \code{order} here~~
}
  \item{align}{specifies whether result should be centered (default), left-aligned or right-aligned.

%%     ~~Describe \code{align} here~~
}
}
\details{The implementation is using the function \code{filter} to calculate the moving average.
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns a numeric vector of the same size as x. 

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{There's a faster implementation of running mean in the package \pkg{caTools} \code{\link[caTools]{runmean}()} and a slower one in \pkg{forecast} \code{\link[forecast]{ma}()}.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
MoveAvg(AirPassengers, order=5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar }
