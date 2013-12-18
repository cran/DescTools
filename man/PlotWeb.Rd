\name{PlotWeb}
\alias{PlotWeb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a Web of Connected Points
%%  ~~function to do ... ~~
}
\description{
This plot can be used to graphically display a correlation matrix by using the linewidth between the nodes
in proportion to the correlation of two variables. It will place the elements homogenously around a circle and draw 
connecting lines between the points. 
}
\usage{
PlotWeb(m, col = c("red", "blue"), lty = par("lty"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{a matrix with numeric values
%%     ~~Describe \code{m} here~~
}
  \item{col}{the color for the connecting lines
%%     ~~Describe \code{col} here~~
}
  \item{lty}{the line type for the connecting lines
%%     ~~Describe \code{lty} here~~
}
  \item{\dots}{dots are passed to plot.
%%     ~~Describe \code{\dots} here~~
}
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{PlotCorr}}
}
\examples{
m <- cor(d.pizza[,WhichNumerics(d.pizza)[-c(1:2)]], use="pairwise.complete.obs")
PlotWeb(m=m, col=c("red","blue"), main="Pizza Correlation")

}
\keyword{ hplot }
\keyword{ multivariate }
