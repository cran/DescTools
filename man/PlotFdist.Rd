\name{PlotFdist}
\alias{PlotFdist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Frequency Distribution Plot
%%  ~~function to do ... ~~
}
\description{
This function is designed to give a univariate graphic representation of a numeric vector's frequency distribution. 
It combines a histogram, a density curve, a boxplot and a plot of the empirical cumulative distribution function (ecdf) in one single plot, resulting in a dense and informative picture of the facts. 
Still the function remains flexible as all possible arguments can be passed to the single components (hist, boxplot etc.) by list (see examples). 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotFdist(x, main = deparse(substitute(x)), xlab = "", xlim = NULL,
          do.hist = !(all(IsWhole(x, na.rm = TRUE)) & length(unique(na.omit(x))) < 13),
          args.hist = NULL, args.rug = NA, args.dens = NULL, args.curve = NA, 
          args.boxplot = NULL, args.ecdf = NULL, heights = NULL,
          pdist = NULL, na.rm = FALSE, cex.axis = NULL, cex.main = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the numerical variable, whose distribution is to be plotted. 
%%     ~~Describe \code{x} here~~
}
  \item{main}{main title of the plot.
%%     ~~Describe \code{main} here~~
}
 \item{xlab}{label of the x-axis, defaults to "". (The name of the variable is typically  placed in the main title and would be redundant.)
%%     ~~Describe \code{xlab} here~~
}
  \item{xlim}{range of the x-axis, defaults to a pretty \code{range(x, na.rm = TRUE)}.
%%     ~~Describe \code{xlim} here~~
}
   \item{do.hist}{defines, whether a histogram or a plot with \code{type = "h"} should be used. 
  Default is \code{TRUE} (meaning a histogram will be plotted), unless x is an integer with less than 13 unique values!
%%     ~~Describe \code{do.hist} here~~
}
  \item{args.hist}{list of additional arguments to be passed to the histogram \code{hist()}, ignored if \code{do.hist = FALSE}.
  The defaults chosen when setting \code{args.hist = NULL} are more or less the same as in \code{\link{hist}}.
%%     ~~Describe \code{args.hist} here~~
}
  \item{args.rug}{list of additional arguments to be passed to the function \code{rug()}.
      Use \code{args.rug = NA} if no rug should be added. This is the default. Use \code{args.rug = NULL} to add rug with reasonable default values. 
  %%     ~~Describe \code{args.rug} here~~
}
  \item{args.dens}{list of additional arguments to be passed to \code{density}.
  Use \code{args.dens = NA} if no density curve should be drawn. The defaults are taken from \code{\link{density}}.
%%     ~~Describe \code{args.dens} here~~
}
  \item{args.curve}{list of additional arguments to be passed to \code{\link{curve}}.
  This argument allows to add a fitted distribution curve to the histogram. By default no curve will be added (args.curve = NA). If the argument is set to NULL, a normal curve with mean(x) and sd(x) will be drawn. See examples for more details.
%%     ~~Describe \code{args.dens} here~~
}
  \item{args.boxplot}{list of additional arguments to be passed to the boxplot \code{boxplot()}.
  The defaults are pretty much the same as in \code{\link{boxplot}}.
%%     ~~Describe \code{args.hist} here~~
}
  \item{args.ecdf}{list of additional arguments to be passed to \code{ecdf()}.
  Use \code{args.ecdf = NA} if no empirical cumulation function should be included in the plot.
  The defaults are taken from \code{\link{plot.ecdf}}.
%%     ~~Describe \code{args.ecdf} here~~
}
  \item{heights}{heights of the plotparts, defaults to \code{c(2,0.5,1.4)} for the histogram, the boxplot 
  and the empirical cumulative distribution function, resp. to \code{c(2,1.5)} for a histogram and a boxplot only.
%%     ~~Describe \code{heights} here~~
}
  \item{pdist}{distances of the plotparts, defaults to \code{c(0, 0)}, say there will be no distance between the histogram, the boxplot and the ecdf-plot. This can be changed for instance in case that the xaxis is to 
  be added to the histogram.
%%     ~~Describe \code{heights} here~~
}
  \item{na.rm}{logical, should NAs be omitted? Histogram and boxplot could do without this option,
  but the density-function refuses to plot with missings. Defaults to \code{FALSE}.
%%     ~~Describe \code{na.rm} here~~
}
\item{cex.axis}{character extension factor for the axes.}
\item{cex.main}{character extension factor for the main title. Must be set in dependence of the plot parts in order to get a harmonic view.}

}

\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{
\code{\link{hist}}, \code{\link{boxplot}}, \code{\link{ecdf}}, \code{\link{density}}, \code{\link{rug}}, \code{\link{layout}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# create a new window and do the plot 
PlotFdist(x=d.pizza$delivery_min, na.rm=TRUE)

# define additional arguments for hist and dens
PlotFdist(d.pizza$delivery_min, args.hist=list(breaks=50), 
  args.dens=list(col="olivedrab4"), na.rm=TRUE )

# do a "h"-plot instead of a histogram for integers 
PlotFdist(d.pizza$weekday, na.rm=TRUE)

# special arguments for ecdf
PlotFdist(x=faithful$eruptions, args.ecdf=list(verticals=FALSE, 
  do.points=TRUE, cex=1.2, pch=16, lwd=1), args.rug=NULL)

# no density curve, no ecdf but add rug instead, make boxplot a bit higher
PlotFdist(x=d.pizza$delivery_min, na.rm=TRUE, args.dens=NA, args.ecdf=NA, 
  args.hist=list(xaxt="s"),  # display x-axis on the histogram
  args.rug=TRUE, heights=c(3, 2.5), pdist=2.5, main="Delivery time")

# alpha channel on rug is cool, but takes its time for being drawn...
PlotFdist(x=d.pizza$temperature, args.rug=list(col=SetAlpha("black", 0.1)), na.rm=TRUE)

# plot a normal density curve
x <- rnorm(1000) 
PlotFdist(x, args.curve = NULL, args.boxplot=NA, args.ecdf=NA)

# compare with a t-distribution
PlotFdist(x, args.curve = list(expr="dt(x, df=2)", col="darkgreen"), 
          args.boxplot=NA, args.ecdf=NA)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }

