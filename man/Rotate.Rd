\name{Rotate}
\alias{Rotate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Rotate a Geometric Structure
%%  ~~function to do ... ~~
}
\description{Rotate a geometric structure by an angle theta around a centerpoint xy.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Rotate(x, y, mx = 0, my = 0, theta = pi/3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{a vector of xy-coordinates of the geometric structure, which has to be rotated
%%     ~~Describe \code{x} here~~
}
  \item{mx,my}{xy-coordinates of the center of the rotation.
%%     ~~Describe \code{mx} here~~
}
  \item{theta}{angle of the rotation
%%     ~~Describe \code{theta} here~~
}
}
\value{The function invisibly returns a list of the coordinates for the rotated shape(s).
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Andri Signorell <andri@signorell.net>
}

\seealso{
\code{\link{polygon}}, \code{\link{DrawRegPolygon}}, \code{\link{DrawEllipse}}, \code{\link{DrawArc}}, \code{\link{DrawAnnulus}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{

# let's have a triangle
Canvas(main="Rotation")
x <- DrawRegPolygon(nv=3)[[1]]

# and rotate
sapply( (0:3) * pi/6, function(theta) {
  xy <- Rotate( x=x$x, y=x$y, theta=theta )
  polygon(xy$x, xy$y, col=SetAlpha("blue", 0.2))
} )

abline(v=0,h=0)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }
