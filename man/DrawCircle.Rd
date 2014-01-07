\name{DrawCircle}
\alias{DrawCircle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draw a Circle
%%  ~~function to do ... ~~
}
\description{Draw one or several circle on an existing plot.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DrawCircle(x = 0, y = x, radius = 1, rot = 0, nv = 100, 
           border = par("fg"), col = par("bg"), lty = par("lty"), 
           lwd = par("lwd"), plot = TRUE)
           
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{a vector (or scalar) of xy-coordinates for the center(s) of the circle(s).
%%     ~~Describe \code{x} here~~
}
  \item{radius}{a scalar or a vector giving the radius of the circle(s)
%%     ~~Describe \code{radius} here~~
}
 \item{rot}{rotation angle for the geometric structure in radiants.}
 
  \item{nv}{number of vertices to draw the circle.
%%     ~~Describe \code{nv} here~~
}
  \item{border}{color for annulus borders. The default is par("fg"). Use border = NA
to omit borders.
%%     ~~Describe \code{border} here~~
}
  \item{col}{color(s) to fill or shade the circle(s) with. The default \code{NA} (or also NULL) means
do not fill, i.e., draw transparent rectangles, unless density is specified.
%%     ~~Describe \code{col} here~~
}
  \item{lty}{line type for borders and shading; defaults to \code{"solid"}.
%%     ~~Describe \code{lty} here~~
}
  \item{lwd}{line width for borders and shading.
%%     ~~Describe \code{lwd} here~~
}
  \item{plot}{logical. If \code{TRUE} the structure will be plotted. If \code{FALSE} only the points are
calculated and returned. Use this option if you want to combine several geometric
structures to a polygon.
%%     ~~Describe \code{plot} here~~
}
}
\details{All geometric arguments will be recycled.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The function invisibly returns a list of the calculated coordinates for all shapes.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{polygon}}, \code{\link{DrawRegPolygon}}, \code{\link{DrawEllipse}}, \code{\link{DrawArc}}, \code{\link{DrawAnnulus}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

Canvas(xlim=c(-5,5))
DrawCircle( radius=4:1, col=c("white","steelblue2","white","red"), lwd=3, nv=300)


x <- seq(-3,3, length.out=18)

par(bg="black")
plot( x=c(-5,5), y=c(-5,5), asp=1, type="n", xaxt="n", yaxt="n", xlab="", ylab="")

sapply( (0:12) * pi/6, function(theta) {
  xy <- Rotate( x, y=0, theta=theta )
  DrawCircle( x=xy$x, y=xy$y, radius=2.4, border="white", col="transparent" )
} )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }

