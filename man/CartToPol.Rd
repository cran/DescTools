\name{CartToPol}
\alias{CartToPol}
\alias{PolToCart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Transform Cartesian to Polar Coordinates and vice versa
%%  ~~function to do ... ~~
}
\description{Transform cartesian into polar coordinates.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CartToPol(x, y)
PolToCart(r, theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{two vectors with the xy-coordianates to be transformed.
%%     ~~Describe \code{x} here~~
}
  \item{r}{a vector with the radius of the points.
%%     ~~Describe \code{x} here~~
}
  \item{theta}{a vector with the angle(s) of the points.
%%     ~~Describe \code{x} here~~
}
}
\details{
Angles are in radians, not degrees (i.e., a right angle is pi/2). Use \code{\link{DegToRad}} to convert, 
if you don't wanna do it by yourself.\cr
All parameters are recycled if necessary.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
PolToCart returns a list of x and y coordinates of the points.\cr
CartToPol returns a list of r for the radius and theta for the angles of the given points.
}
\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\examples{

CartToPol(x=1, y=1)
CartToPol(x=c(1,2,3), y=c(1,1,1))
CartToPol(x=c(1,2,3), y=1)


PolToCart(r=1, theta=pi/2)
PolToCart(r=c(1,2,3), theta=pi/2)



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{arith}
\keyword{dplot}
