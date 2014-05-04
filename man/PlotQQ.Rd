\name{PlotQQ}
\alias{PlotQQ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{QQ-Plot for Optional Distributions
%%  ~~function to do ... ~~
}
\description{Create a QQ-plot for a variable which is not normally distributed. 
The assumed underlying distribution can freely be defined.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotQQ(x, qdist, ..., main = NULL, xlab = NULL, ylab = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the data sample
%%     ~~Describe \code{x} here~~
}
  \item{qdist}{the quantile function of the assumed distribution as text or as function.
%%     ~~Describe \code{qdist} here~~
}
  \item{\dots}{optional arguments to be passed to the quantile function
%%     ~~Describe \code{\dots} here~~
}
  \item{main}{the main title for the plot. This will be "Q-Q-Plot" by default
%%     ~~Describe \code{main} here~~
}
  \item{xlab}{the xlab for the plot
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{the ylab for the plot
%%     ~~Describe \code{ylab} here~~
}
}
\details{The function generates a sequence of points between 0 and 1 and transforms those
into quantiles by means of the defined assumed distribution.
%%  ~~ If necessary, more details than the description above ~~
}
\note{ The code is based on the tip 10.22 "Creating Ohrer Quantile-Quantile Plots" from R Cookbook.}
\references{Teetor, P. (2011) \emph{R Cookbook}. O'Reilly, pp. 254-255.}

\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{qqnorm}}, \code{\link{qqline}}, \code{\link{qqplot}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
y <- rexp(100, 1/10)
PlotQQ(y, qexp, rate=1/10)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
\keyword{ hplot }

