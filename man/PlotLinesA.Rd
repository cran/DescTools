\name{PlotLinesA}
\alias{PlotLinesA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Lines 
%%  ~~function to do ... ~~
}
\description{Plot the columns of one matrix against the columns of another. Adds a legend on the right at the endpoints of lines. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotLinesA(x, y, col = 1:5, lty = 1, lwd = 1, lend = par("lend"), 
           xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, 
           cex = 1, cex.legend = 1, main = NULL, grid = TRUE, mar = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{vectors or matrices of data for plotting. The number of rows should match. If one of them are missing, the other is taken as y and an x vector of \code{1:n} is used. Missing values (\code{NAs}) are allowed.
%%     ~~Describe \code{x} here~~
}
  \item{col}{vector of colors. Colors are used cyclically.
%%     ~~Describe \code{col} here~~
}
  \item{lty, lwd, lend}{vector of line types, widths, and end styles. The first element is for the first column, the second element for the second column, etc., even if lines are not plotted for all columns. Line types will be used cyclically until all plots are drawn.
%%     ~~Describe \code{lty} here~~
}
  \item{xlab, ylab}{titles for x and y axes, as in \link{plot}.
%%     ~~Describe \code{xlab} here~~
}
  \item{xlim, ylim}{ranges of x and y axes, as in \link{plot}.
%%     ~~Describe \code{xlim} here~~
}
  \item{cex, cex.legend}{character expansion factor relative to current \code{par("cex")}. Can be set for matplot and for the legend text independently.
%%     ~~Describe \code{xlim} here~~
}
  \item{main}{an overall title for the plot.
%%     ~~Describe \code{main} here~~
}
  \item{grid}{logical adds an nx by ny rectangular grid to an existing plot.
%%     ~~Describe \code{grid} here~~
}
  \item{mar}{the margins of the plot.
%%     ~~Describe \code{mar} here~~
}
}
\details{This function is rather a template, than a function. It wraps \code{\link{matplot}} to generate a lines plot and adds a rather sophisticated legend on the right side, while calculating appropriate margins. A grid option is included (as \code{panel.first} does not work in matplot).

As in matplot, the first column of x is plotted against the first column of y, the second column of x against the second column of y, etc. If one matrix has fewer columns, plotting will cycle back through the columns again. (In particular, either x or y may be a vector, against which all columns of the other argument will be plotted.)
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{matplot}}, \code{\link{par}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
m <- matrix(c(3,4,5,1,5,4,2,6,2), nrow = 3, 
            dimnames = list(dose = c("A","B","C"), 
                            age  = c("2010","2011","2012")))
                            
PlotLinesA(m, col=c(PalHelsana()), main="Dose ~ age_grp", lwd=3, ylim=c(1, 10))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
