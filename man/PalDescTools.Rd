\name{PalDescTools}
\alias{PalDescTools}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Some Custom Palettes
%%  ~~function to do ... ~~
}
\description{Some more custom palettes.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PalDescTools(pal, n = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pal}{name or number of the palette. One of 
  \code{RedToBlack (1)}, \code{RedBlackGreen (2)}, \code{SteeblueWhite (3)}, \code{RedWhiteGreen (4)},
                \code{RedWhiteBlue1 (5)}, \code{RedWhiteBlue2 (6)}, \code{Helsana (7)}, \code{Tibco (8)}

%%     ~~Describe \code{pal} here~~
}
  \item{n}{integer, number of colors for the palette.
%%     ~~Describe \code{n} here~~
}
}
\value{a vector of colors
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{colorRampPalette}}
}
\examples{

Canvas(c(0,1))
ColorLegend(x=0, y=1, width=0.1, col=PalDescTools(1, n=50))
ColorLegend(x=0.15, y=1, width=0.1, col=PalDescTools(2, n=50))
ColorLegend(x=0.3, y=1, width=0.1, col=PalDescTools(3, n=50))
ColorLegend(x=0.45, y=1, width=0.1, col=PalDescTools(4, n=50))
ColorLegend(x=0.6, y=1, width=0.1, col=PalDescTools(5, n=50))
ColorLegend(x=0.75, y=1, width=0.1, col=PalDescTools(6, n=50))
ColorLegend(x=0.9, y=1, width=0.1, col=PalDescTools(7))
ColorLegend(x=1.05, y=1, width=0.1, col=PalDescTools(8))

text(1:8, y=1.05, x=seq(0,1.05,.15)+.05)
title(main="DescTools palettes")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ color}