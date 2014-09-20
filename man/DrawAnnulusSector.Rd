\name{DrawAnnulusSector}
\alias{DrawAnnulusSector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draw a Sector of an Annulus
%%  ~~function to do ... ~~
}
\description{Draw one or more annulus sectors with given centers, radii, angles, 
fill- and border colors on an existing plot using classical graphics. 


%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DrawAnnulusSector(x = 0, y = x, radius.in = 1, radius.out = 2, 
                  angle.beg = 0, angle.end = pi, nv = 100, 
                  border = par("fg"), col = par("bg"), lty = par("lty"), 
                  lwd = par("lwd"), plot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{a vector (or scalar) of xy-coordinates of the center(s). 
%%     ~~Describe \code{x} here~~
}
  \item{radius.in}{a vector (or scalar) of the inner radius of the annulus(i). 
%%     ~~Describe \code{radius.in} here~~
}
  \item{radius.out}{a vector (or scalar) of the outer radius of the annulus(i). 
%%     ~~Describe \code{radius.out} here~~
}
  \item{angle.beg}{a vector (or scalar) of the starting angle(s). The sectors are built counterclockwise.
%%     ~~Describe \code{angle.beg} here~~
}
  \item{angle.end}{a vector (or scalar) of the ending angle(s).
%%     ~~Describe \code{angle.end} here~~
}
  \item{nv}{number of vertices to draw the arcs.
%%     ~~Describe \code{nv} here~~
}
  \item{border}{color for borders. The default is \code{par("fg")}. 
    Use \code{border = NA} to omit borders. If there are shading lines, \code{border = TRUE} means use the 
    same colour for the border as for the shading lines.
}
  \item{col}{color(s) to fill or shade the annulus sector with. The default NA (or also NULL) means do not fill (say draw transparent). 
%%     ~~Describe \code{col} here~~
}
  \item{lty}{line type for borders and shading; defaults to \code{"solid"}. 
%%     ~~Describe \code{lty} here~~
}
  \item{lwd}{line width for borders and shading. 
%%     ~~Describe \code{lwd} here~~
}
  \item{plot}{logical. If TRUE the structure will be plotted. If FALSE only the points are calculated and returned. 
   Use this if you want to combine several geometric structures to a single polygon.
%%     ~~Describe \code{plot} here~~
}
}
\value{\code{DrawAnnulusSector} invisibly returns a list of the calculated coordinates for all shapes. 
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}


\seealso{\code{\link{polygon}}, \code{\link{DrawAnnulus}}, \code{\link{DrawRegPolygon}}, \code{\link{DrawCircle}}, \code{\link{DrawArc}}
%% ~~objects to See Also as , ~~~
}
\examples{
par(mfrow=c(1,2))

angles <- seq( 0,2 * pi, pi/4)  # the angles
mycol <- rainbow(8)             # colors of the sector annuli
d <- 0.1                        # the gap between the sectors in radians

plot(1:10, type="n", asp=1, xlab="", ylab="")
res <- sapply( 1:(length(angles)-1), 
  function(i) DrawAnnulusSector(x = 6, y = 6, radius.in = 2, radius.out = 3, 
                angle.beg = angles[i] + d/2, angle.end = angles[i+1] - d/2, col = mycol[i])
)


# Produce a clockplot
x <- c(15,9,75,90,1,1,11,5,9,8,33,11,11,20,14,13,10,28,33,21,24,25,11,33)
# plot clockwise, starting from 12 o'clock
angles <- (rev(seq(0,2*pi, pi/12) + pi/2))

Canvas(xlim=c(-100,100), main="Number of visitors to web site for each hour of a day")
PolarGrid(nr=c(0,90), ntheta=24, rlabels=NA, alabels=c(6:0, 23:7) )      
DrawAnnulusSector(radius.in=0, radius.out=x, angle.beg = angles[-1], 
  angle.end = angles[-length(angles)], col=rainbow(24))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }

