\name{DrawAnnulus}
\alias{DrawAnnulus}

\title{Draw One or Several Annuli

}
\description{Draw an annulus (or a sequence of annuli) with given center coordinates, 
 fill and border colors on an existing plot using classical graphics. 

}
\usage{
DrawAnnulus(x = 0, y = x, radius.in = 1, radius.out = 2, nv = 100, 
            border = par("fg"), col = par("bg"), lty = par("lty"), 
            lwd = par("lwd"), plot = TRUE)
}
\arguments{
  \item{x, y }{a vector (or scalar) of xy-coordinates of the center(s).
}
  \item{radius.in}{a vector (or scalar) of the inner radius of the annulus(i).
}
  \item{radius.out}{a vector (or scalar) of the outer radius of the annulus(i).
}
  \item{nv}{number of vertices to draw the circles. Default is 100.
}
  \item{border}{color for the border(s). The default is \code{par("fg")}. 
    Use \code{border = NA} to omit borders. If there are shading lines, \code{border = TRUE} means use the 
    same colour for the border as for the shading lines.

}
  \item{col}{color(s) to fill or shade the annulus with. The default \code{NA} (or also NULL) means do not 
   fill, i.e., draw transparent rectangles, unless \code{density} is specified.
}
  \item{lty}{line type for borders and shading; defaults to \code{"solid"}.
}
  \item{lwd}{line width for borders and shading.
}
  \item{plot}{logical. If \code{TRUE} the structure will be plotted. If \code{FALSE} only the xy-points are calculated and returned.
    Use this option if you want to combine several geometric structures to a single polygon.
}
}
\details{All geometric arguments are recycled if necessary.
}
\value{\code{DrawAnnulus} invisibly returns a list of the calculated coordinates for all shapes. 
}

\author{ Andri Signorell <andri@signorell.net>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{polygon}}, \code{\link{DrawRegPolygon}}, \code{\link{DrawCircle}}, \code{\link{DrawArc}}
}
\examples{
Canvas(0.5)
DrawRegPolygon(nv=4, rot=pi/4, col="lightblue")
DrawAnnulus(radius.in=0.3, radius.out=0.45, col="lightgrey", border="darkgrey", lwd=5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }
