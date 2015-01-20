\name{PlotBubble}
\alias{PlotBubble}
\alias{PlotBubble.default}
\alias{PlotBubble.formula}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draw a Bubble Plot
%%  ~~function to do ... ~~
}
\description{Draw a bubble plot, defined by a pair of coordinates x, y to place the bubbles, an area 
definition configuring the dimension and a color vector setting the color of the bubbles.
The legitimation to define a new function instead of just using \code{plot(symbols(...))} is the automated calculation of the axis limits, ensuring that 
all bubbles will be fully visible.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{

PlotBubble(x, ...)

\method{PlotBubble}{default}(x, y, area, col, border = NA, na.rm = FALSE, inches = FALSE, ...)

\method{PlotBubble}{formula}(formula, data = parent.frame(), ..., subset, ylab = varnames[response])

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{the x and y co-ordinates for the centres of the bubbles. They can be specified in any way which is accepted by \code{\link{xy.coords}}.
%%     ~~Describe \code{x} here~~
}
  \item{area}{a vector giving the area of the bubbles.
%%     ~~Describe \code{area} here~~
}
  \item{col}{colors for the bubbles, passed to \code{\link{symbol}}.
%%     ~~Describe \code{col} here~~
}
  \item{border}{the border color fot the bubbles. Set \code{NA} if there should be no border at all. 
  This is the default.
%%     ~~Describe \code{border} here~~
}
  \item{na.rm}{logical, should NAs be omitted? Defaults to FALSE.
%%     ~~Describe \code{na.rm} here~~
}
  \item{inches}{\code{TRUE}, \code{FALSE} or a positive number. See 'Details'.
%%     ~~Describe \code{inches} here~~
}

  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the data values and rhs the corresponding groups.}  
  \item{data}{an optional matrix or data frame (or similar: see \code{\link{model.frame}}) containing the variables in the formula \code{formula}. 
    By default the variables are taken from \code{environment(formula)}.}  
  \item{subset}{an optional vector specifying a subset of observations to be used.}  
\item{ylab}{the y-label for the plot used in the formula interface.}

  \item{\dots}{the dots are passed to the \code{\link{plot}} function.
%%     ~~Describe \code{\dots} here~~
}
}
\details{
Argument \code{inches} controls the sizes of the symbols. If \code{TRUE} (the default), the symbols are scaled so that the largest dimension of any symbol is one inch. If a positive number is given the symbols are scaled to make largest dimension this size in inches (so \code{TRUE} and \code{1} are equivalent). If \code{inches} is \code{FALSE}, the units are taken to be those of the appropriate axes. 
This behaviour is the same as in \code{\link{symbols}}.

%%  ~~ If necessary, more details than the description above ~~
}
\note{A legend can be added with \code{\link{BubbleLegend}}.}

\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{BubbleLegend}}, \code{\link{symbols}}, \code{\link{sunflowerplot}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

PlotBubble(x=d.pizza$delivery_min, y=d.pizza$temperature, area=d.pizza$price/40, 
           xlab="delivery time", ylab="temperature", 
           col=SetAlpha(as.numeric(d.pizza$area)+2, .5), border="darkgrey", 
           na.rm=TRUE, main="Price-Bubbles", panel.first=grid())

BubbleLegend("bottomleft", frame=TRUE, cols=SetAlpha("steelblue",0.5), bg="green",
             radius = c(10, 5, 2)/2, labels=c(10, 5, 2), cex=0.8,
             cols.lbl=c("yellow", "red","blue"), width=12)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
