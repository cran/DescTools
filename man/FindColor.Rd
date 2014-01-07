\name{FindColor}
\alias{FindColor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get Color on a Defined Color Range
%%  ~~function to do ... ~~
}
\description{Find a color on a defined color range depending on the value of x. This is helpful for colorcoding numeric values.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
FindColor(x, cols = rev(heat.colors(100)), 
          min.x = min(pretty(x)), max.x = max(pretty(x)))

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric.
%%     ~~Describe \code{x} here~~
}
  \item{cols}{all the colors in defined range.
%%     ~~Describe \code{cols} here~~
}
  \item{min.x}{the x-value for the first color.
%%     ~~Describe \code{col_start} here~~
}
  \item{max.x}{the x-value for the last color.
%%     ~~Describe \code{col_end} here~~
}
}
\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{findInterval}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

Canvas(7, main="Use of function FindColor()")

# get some data
x <- c(23,56,96)
# get a color range from blue via white to red
cols <- colorRampPalette(c("blue","white","red"))(100)  
ColorLegend(x="bottomleft", cols=cols, xlab=rev(seq(0,100,10)), cex=0.8)

# and now the color coding of x:
xcols <- FindColor(x, cols, min.x=0, max.x=100 )

# how does it look like?
text(x=1, y=c(3), labels="Color coding of x:")
text(x=1.5, y=c(-5,-2,1), labels=x)
DrawRegPolygon(x=3, y=c(-5,-2,1), nv=4, rot=pi/4, col=xcols)
text(x=6, y=c(-5,-2,1), labels=xcols)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ color }
\keyword{ dplot }