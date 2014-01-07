\name{AddErrBars}
\alias{AddErrBars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add Error Bars to an Existing Plot
%%  ~~function to do ... ~~
}
\description{Add Error Bars to an Existing Plot.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AddErrBars(from, to = NULL, pos = NULL, mid = NULL, horiz = FALSE, col = par("fg"), 
           lty = par("lty"), lwd = par("lwd"), code = 3, length = 0.05,
           pch = NA, cex.pch = par("cex"), col.pch = par("fg"), bg.pch = par("bg") )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{from}{coordinates of points \bold{from} which to draw (the lower end of the error bars).
%%     ~~Describe \code{x0} here~~
}
  \item{to}{coordinates of points \bold{to} which to draw (the upper end of the error bars).
%%     ~~Describe \code{x1} here~~
}
  \item{pos}{numeric, position of the error bars. This will either be the x-coordinate in case of vertical error bars
  and the y-coordinate in case of horizontal error bars.
%%     ~~Describe \code{y} here~~
}
  \item{mid}{numeric, position of midpoints. Defaults to the mean of from and to.
%%     ~~Describe \code{col} here~~
}
  \item{horiz}{boolean, TRUE (default) if horizontal error bars are needed.
%%     ~~Describe \code{col} here~~
}
  \item{col}{the line color.
%%     ~~Describe \code{col} here~~
}
  \item{lty}{the line type.
%%     ~~Describe \code{lty} here~~
}
  \item{lwd}{line width.
%%     ~~Describe \code{lwd} here~~
}
  \item{code}{integer code, determining kind of arrows to be drawn.
  If code = 1 an arrowhead is drawn at (x0[i], y0[i]) and if code = 2 an arrowhead is drawn at (x1[i], y1[i]). 
  If code = 3 (default) a head is drawn at both ends of the arrow. Unless length = 0, when no head is drawn.
%%     ~~Describe \code{code} here~~
}
  \item{length}{the length of the end lines. 
%%     ~~Describe \code{length} here~~
}
  \item{pch}{plotting character for the midpoints. No points will be plotted if this is set to NA, 
  which is the default.
}
  \item{cex.pch}{the character extension for the plotting characters. Default is par("cex")}
  \item{col.pch}{the color of the plotting characters. Default is par("fg")}
  \item{bg.pch}{the background color of the plotting characters (if pch is set to 21:25). Default is par("bg")}

  \item{\dots}{the dots are passed to the \code{\link{arrows}} function.
%%     ~~Describe \code{\dots} here~~
}
}
\details{A short wrapper for plotting error bars by means of \code{\link{arrows}}.
%%  ~~ If necessary, more details than the description above ~~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{AddLoess}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

par(mfrow=c(2,2))
b <- barplot(1:5, ylim=c(0,6))
AddErrBars(from=1:5-rep(0.5,5), to=1:5+rep(0.5,5), pos=b, length=0.3)

# just on one side
b <- barplot(1:5, ylim=c(0,6))
AddErrBars(from=1:5, to=1:5+rep(0.5,5), pos=b, length=0.2, col="red", code=2, lwd=2)

b <- barplot(1:5, xlim=c(0,6), horiz=TRUE)
AddErrBars(from=1:5, to=1:5+rep(0.2,5), pos=b, horiz=TRUE,  length=0.2, col="red", code=2, lwd=2)

par(xpd=FALSE)
dotchart(1:5, xlim=c(0,6))
AddErrBars(from=1:5-rep(0.2,5), to=1:5+rep(0.2,5), horiz=TRUE, length=0.1)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }
