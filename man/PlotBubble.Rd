\name{PlotBubble}
\alias{PlotBubble}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draw a Bubble Plot
%%  ~~function to do ... ~~
}
\description{Draw a bubble plot, defined by a pair of coordinates x, y to place the bubbles, an area 
definition configuring the dimension and a color vector setting the color of the bubbles.
The legitimation to define a new function instead of just using \code{plot(symbols(...))} is the automated calculation of the axis limits, ensuring
all elements will be fully visible.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotBubble(x, y, area, col, border = NA, na.rm = FALSE, inches = FALSE, ...)
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
  \item{\dots}{the dots are passed to the \code{\link{plot}} function.
%%     ~~Describe \code{\dots} here~~
}
}
\details{
Argument \code{inches} controls the sizes of the symbols. If \code{TRUE} (the default), the symbols are scaled so that the largest dimension of any symbol is one inch. If a positive number is given the symbols are scaled to make largest dimension this size in inches (so \code{TRUE} and \code{1} are equivalent). If \code{inches} is \code{FALSE}, the units are taken to be those of the appropriate axes. 
This behaviour is the same as in \code{\link{symbols}}.
%%  ~~ If necessary, more details than the description above ~~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{symbols}}, \code{\link{sunflowerplot}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

PlotBubble( x=d.pizza$delivery_min, y=d.pizza$temperature, area=d.pizza$price, 
  xlab="delivery time", ylab="temperature", 
  col=SetAlpha(as.numeric(d.pizza$area)+2, .5), border="darkgrey", 
  na.rm=TRUE, main="Price-Bubbles", panel.first=grid())


PlotBubble(d.world$x, d.world$y, area=d.world$pop/90, col=SetAlpha("steelblue",0.4), 
  border="darkblue", xlab="", ylab="", panel.first=grid(), 
  main="World population")
text(d.world$x, d.world$y, labels=d.world$country, cex=0.7, adj=0.5)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
