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
PlotWeb(m, col = c("red", "blue"), lty = par("lty"), args.legend=NULL, 
        pch = 21, pt.cex = 2, pt.col = "black", pt.bg = "darkgrey", ...)
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
  \item{args.legend}{list of additional arguments to be passed to the \code{legend} function. 
  Use \code{args.legend = NA} if no legend should be added. 
%%     ~~Describe \code{args.legend} here~~
}
  \item{pch}{the plotting symbols appearing in the plot, as a non-negative numeric vector (see \code{\link{points}}, but unlike there negative values are omitted) or a vector of 1-character strings, or one multi-character string.
}
  \item{pt.cex}{expansion factor(s) for the points.
}
  \item{pt.col}{the foreground color for the points, corresponding to its argument \code{col}.
}
  \item{pt.bg}{the background color for the points, corresponding to its argument \code{bg}.
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
