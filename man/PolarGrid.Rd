\name{PolarGrid}
\alias{PolarGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a Grid in Polar Coordinates
%%  ~~function to do ... ~~
}
\description{\code{PolarGrid} adds a polar grid to an existing plot. The number of radial gridlines are
set by \code{ntheta} and the tangential lines by \code{nr}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PolarGrid(nr = NULL, ntheta = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), 
          rlabels = NULL, alabels = NULL, lblradians = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nr}{number of circles. When NULL, as per default, the grid aligns with the tick marks on the corresponding default axis 
   (i.e., tickmarks as computed by axTicks). When NA, no circular grid lines are drawn.

%%     ~~Describe \code{nr} here~~
}
  \item{ntheta}{number of radial grid lines. Defaults to 12 uniformly distributed between 0 and 2*pi (each pi/3). 
%%     ~~Describe \code{ntheta} here~~
}
  \item{col}{character or (integer) numeric; color of the grid lines.
%%     ~~Describe \code{col} here~~
}
  \item{lty}{character or (integer) numeric; line type of the grid lines.

%%     ~~Describe \code{lty} here~~
}
  \item{lwd}{non-negative numeric giving line width of the grid lines.

%%     ~~Describe \code{lwd} here~~
}
   \item{rlabels}{the radius labels. Use NA if no labels should be to be added.
%%     ~~Describe \code{rlabels} here~~
}
  \item{alabels}{the labels for the angles, they are printed on a circle outside the plot. Use NA for no angle labels.
%%     ~~Describe \code{anglabels} here~~
}
  \item{lblradians}{logic, defines if angle labels will be in degrees (default) or in radians.
%%     ~~Describe \code{lblradians} here~~
}
}
\details{This can be made better....
%%  ~~ If necessary, more details than the description above ~~
}
\author{Andri Signorell <andri@signorell.net> }


\seealso{\code{\link{PlotPolar}}}
\examples{
Canvas(xlim=c(-5,5), xpd=TRUE)
PolarGrid()

Canvas(xlim=c(-5,5), xpd=TRUE)
PolarGrid(nr=0:5, ntheta=6)

Canvas(xlim=c(-5,5), xpd=TRUE)
PolarGrid(ntheta=36, rlabels=NA, lblradians=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }
