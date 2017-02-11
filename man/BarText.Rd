\name{BarText}
\alias{BarText}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add Text to a Barplot
%%  ~~function to do ... ~~
}
\description{Places some text directly on the (stacked) bars of a barplot.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BarText(height, b, labels = height, beside = FALSE, horiz = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{height}{either a vector or matrix of values describing the bars which make up the plot exactly as used for creating the barplot.
%%     ~~Describe \code{height} here~~
}
  \item{b}{the returned mid points as returned by \code{b <- barplot(...)}.
%%     ~~Describe \code{b} here~~
}
  \item{labels}{the labels to be placed on the bars.
%%     ~~Describe \code{labels} here~~
}
  \item{beside}{a logical value. If FALSE, the columns of height are portrayed as stacked bars, and if TRUE the columns are portrayed as juxtaposed bars.
%%     ~~Describe \code{beside} here~~
}
  \item{horiz}{a logical value. If FALSE, the bars are drawn vertically with the first bar to the left. If TRUE, the bars are drawn horizontally with the first at the bottom.
%%     ~~Describe \code{horiz} here~~
}
  \item{\dots}{the dots are passed to the \code{\link{text}}.
%%     ~~Describe \code{\dots} here~~
}
}
\details{It sometimes makes sense to place the results directly on the bars.
%%  ~~ If necessary, more details than the description above ~~
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{help}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
b <- barplot(VADeaths)
BarText(VADeaths, b=b)

b <- barplot(VADeaths, horiz = TRUE)
BarText(VADeaths, b=b, horiz = TRUE, col="red", cex=1.5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }% use one of  RShowDoc("KEYWORDS")
