\name{PlotDotCI}
\alias{PlotDotCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a Dotchart with Confidence Intervals
%%  ~~function to do ... ~~
}
\description{Plot a dotchart with confidence intervals as segments.
The reason for creating a new function for the job is the automated calculation of the axis limits, ensuring all error bars will be fully visible.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotDotCI(x, xlim = NULL, 
          pch = 21, pch.cex = 1, pch.col = "black", pch.bg = "grey50",
          lcol = "grey40", lwd = 2,
          args.legend = NULL, code = 3, lend = "butt", 
          mar = c(7.1, 4.1, 4.1, 2.1), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{matrix with 3 columns, the first is used as x-values, the second is the left bound and the 3rd the right bound of the segment. 
%%     ~~Describe \code{x} here~~
}
  \item{xlim}{the x limits of the plot.
%%     ~~Describe \code{xlim} here~~
}
  \item{pch}{a vector of plotting characters or symbols.
%%     ~~Describe \code{pch} here~~
}
  \item{pch.cex}{magnification to be used for plotting characters relative to the current setting of cex.
%%     ~~Describe \code{pch.cex} here~~
}
  \item{pch.col}{the colors for points. If using 21 etc. this is the margins color of the point character.
%%     ~~Describe \code{pch.col} here~~
}
  \item{pch.bg}{the colors for points. If using 21 etc. this is the fill color of the point character.
%%     ~~Describe \code{pch.bg} here~~
}
  \item{lcol}{the colors for lines. 
%%     ~~Describe \code{lcol} here~~
}
  \item{lwd}{the widths for the segments.
%%     ~~Describe \code{lwd} here~~
}
  \item{lend}{the line end style. This can be specified as an integer or string, \code{0: "round"}, \code{1: "butt"}, \code{2: "square"}
%%     ~~Describe \code{lwd} here~~
}
  \item{args.legend}{list of additional arguments to be passed to the \code{legend} function. 
  Use args.legend = \code{NA} if no legend should be added. 
%%     ~~Describe \code{args.legend} here~~
}
  \item{code}{integer value. Determines the kind of arrows to be drawn. \code{code = 1} means that a lower arrowhead will be drawn, 
  \code{code = 2} will produce one on the right side and \code{code = 3} on both sides. (See the function \code{\link{arrows}}).
}
  \item{mar}{numeric vector with margins defined.
%%     ~~Describe \code{mar} here~~
}
  \item{\dots}{further arguments are passed to the function \code{\link{dotchart}}.
%%     ~~Describe \code{\dots} here~~
}
}

%%\details{ No details so far....
%%}


\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{PlotDotCIp}}
}
\examples{
xci <- do.call(rbind, tapply( d.pizza$delivery_min, d.pizza$driver, 
  MeanCI, conf.level=0.99, na.rm=TRUE))

PlotDotCI(xci, main="delivery_min ~ driver", 
  args.legend=list(y=-1.5, legend=c("estimate", "99\%-CI")))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }

