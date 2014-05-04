\name{PlotCorr}
\alias{PlotCorr}

\title{Plot a Correlation Matrix
}

\description{This function produces a graphical display of a correlation matrix.
The cells of the matrix can be shaded or colored to show the correlation value.
}
\usage{
PlotCorr(x, cols = colorRampPalette(c("red", "white", "blue"), space = "rgb")(20), 
         breaks = seq(-1, 1, length = length(cols) + 1), 
         border = NA, lwd = 1,
         args.colorlegend = NULL, xaxt = par("xaxt"), yaxt = par("yaxt"), 
         cex.axis = 0.8, las = 2, mar = c(3, 8, 8, 8), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{x is a correlation matrix to be visualized.
%%     ~~Describe \code{x} here~~
}
  \item{cols}{the colors for shading the matrix
%%     ~~Describe \code{cols} here~~
}
 \item{breaks}{a set of breakpoints for the colours: must give one more breakpoint than colour. These are passed to \code{image()} function.
 If breaks is specified then the algorithm used follows \code{\link{cut}}, so intervals are closed on the right and open on the left except for the lowest interval.
%%     ~~Describe \code{cols} here~~
}
  \item{border}{color for borders. The default is \code{NA}, which means borders are omitted.
}
  \item{lwd}{line width for borders. Default is 1.
%%     ~~Describe \code{lwd} here~~
}
\item{args.colorlegend}{list of arguments for the \code{\link{ColorLegend}}. Use NA if no color legend should be painted.
%%    
}
  \item{xaxt}{parameter to define, whether to draw an x-axis, defaults to \code{"n"}.
%%     ~~Describe \code{xaxt} here~~
}
  \item{yaxt}{parameter to define, whether to draw an y-axis, defaults to \code{"n"}.
%%     ~~Describe \code{yaxt} here~~
}
  \item{cex.axis}{character extension for the axis labels.
%%     ~~Describe \code{cex.axis} here~~
}
  \item{las}{the style of axis labels.
%%     ~~Describe \code{las} here~~
}
  \item{mar}{sets the margins, defaults to mar = c(3, 8, 8, 8) as we need a bit more room on the right.
%%     ~~Describe \code{mar} here~~
}
  \item{\dots}{the dots are passed to the function \code{\link{image}}, which produces the plot.
%%     ~~Describe \code{\dots} here~~
}
}
\value{no values returned.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{image}}, \code{\link{ColorLegend}}, \code{\link[corrgram]{corrgram}()} 
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
m <- cor(d.pizza[,WhichNumerics(d.pizza)], use="pairwise.complete.obs")

PlotCorr(m, cols=colorRampPalette(c("red", "black", "green"), space = "rgb")(20))
PlotCorr(m, cols=colorRampPalette(c("red", "black", "green"), space = "rgb")(20), 
         args.colorlegend=NA)

m <- PairApply(d.diamonds[,WhichFactors(d.diamonds)], CramerV)
PlotCorr(m, cols = colorRampPalette(c("white", "steelblue"), space = "rgb")(20),
         breaks=seq(0, 1, length=21), border="black",
         args.colorlegend = list(labels=sprintf("\%.1f", seq(1, 0, length = 11)), frame=TRUE)
)
title(main="Cramer's V", line=2)
text(x=rep(1:ncol(m),ncol(m)), y=rep(1:ncol(m),each=ncol(m)), 
     label=sprintf("\%0.2f", m[,ncol(m):1]), cex=0.8, xpd=TRUE)

# Spearman correlation on ordinal factors
csp <- cor(data.frame(lapply(d.diamonds[,c("carat", "clarity", "cut", "polish", 
                      "symmetry", "price")], as.numeric)), method="spearman")
PlotCorr(csp)

# some more colors
PlotCorr(cor(mtcars), col=PalDescTools("RedWhiteBlue1", 100), border="grey",
         args.colorlegend=list(labels=FormatFix(seq(1,-1,-.25), 2), frame="grey"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\keyword{ multivariate }
