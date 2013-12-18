\name{DrawArc}
\alias{DrawArc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draw Elliptic or Circular Arc(s)
%%  ~~function to do ... ~~
}
\description{ Draw one or more elliptic or circular arcs from \code{angle.beg} to \code{angle.end} on an existing plot using classic graphics.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DrawArc(x = 0, y = x, radius.x = 1, radius.y = radius.x, 
        angle.beg = 0, angle.end = pi, nv = 100, 
        col = par("col"), lty = par("lty"), lwd = par("lwd"), 
        plot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{a vector (or scalar) of xy-coordinates of the center(s) of the arc(s). 
%%     ~~Describe \code{x} here~~
}
  \item{radius.x}{a scalar or a vector giving the semi-major axis of the ellipse for the arc(s)
%%     ~~Describe \code{radius.x} here~~
}
  \item{radius.y}{a scalar or a vector giving the semi-minor axis of the ellipse for the arc(s). 
  Default is radius.x which will result in a circle arc with radius.x.
%%     ~~Describe \code{radius.y} here~~
}
  \item{angle.beg}{a scalar or a vector of starting angles in radians. 
%%     ~~Describe \code{angle.beg} here~~
}
  \item{angle.end}{a scalar or a vector of ending angles in radians. 
%%     ~~Describe \code{angle.end} here~~
}
  \item{nv}{number of vertices used to plot the arc. Scalar or vector.
%%     ~~Describe \code{nv} here~~
}
  \item{col}{color for the arc(s). Scalar or vector. 
%%     ~~Describe \code{col} here~~
}
  \item{lty}{line type used for drawing.

%%     ~~Describe \code{lty} here~~
}
  \item{lwd}{line width used for drawing.
%%     ~~Describe \code{lwd} here~~
}
  \item{plot}{logical. If TRUE the structure will be plotted. If FALSE only the xy-points are calculated and returned.
    Use this if you want to combine several geometric structures to a single polygon.
}

}
\details{
All parameters are recycled if necessary. \cr
Be sure to use an aspect ratio of 1 as shown in the example to avoid distortion.

}
\value{\code{DrawArc} invisibly returns a list of the calculated coordinates for all shapes.

}

\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{DrawCircle}}, \code{\link{DrawAnnulusSector}}, \code{\link{polygon}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

curve(sin(x), 0, pi, col="blue", asp=1)
DrawArc( x = pi/2, y = 0, radius.x = 1, angle.beg = pi/4, angle.end = 3*pi/4, col="red")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }
