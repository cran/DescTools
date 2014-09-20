\name{DescTools Palettes}
\alias{PalDescTools}

\alias{PalTibco}
\alias{PalRedToBlack}
\alias{PalRedBlackGreen}
\alias{PalSteeblueWhite}
\alias{PalRedWhiteGreen}
\alias{PalHelsana}
\alias{hblue}
\alias{hred}
\alias{horange}
\alias{hgreen}
\alias{hyellow}
\alias{hecru}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Some Custom Palettes
%%  ~~function to do ... ~~
}
\description{Some more custom palettes.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PalDescTools(pal, n = 100)

PalTibco()
PalHelsana()
PalRedToBlack(n = 100)
PalRedBlackGreen(n = 100)
PalSteeblueWhite(n = 100)
PalRedWhiteGreen(n = 100)

hblue
hred
horange
hgreen
hyellow

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
\details{hblue and hred are 2 constants, pointing to the red and blue from the palette PalHelsana.
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

par(mfrow=c(4,2), mar=c(1,1,2,1))
barplot(1:9, col=PalTibco(), axes=FALSE, main="PalTibco" ) 

barplot(1:7, col=PalHelsana(), axes=FALSE, main="PalHelsana" )
barplot(1:7, col=SetAlpha(PalHelsana()[c("ecru","hellgruen","hellblau")], 0.6), 
        axes=FALSE, main="PalHelsana (Alpha)" )

barplot(1:10, col=PalRedToBlack(10), axes=FALSE, main="PalRedToBlack" )
barplot(1:10, col=PalRedBlackGreen(10), axes=FALSE, main="PalRedGreenGreen" )
barplot(1:10, col=PalSteeblueWhite(10), axes=FALSE, main="PalSteeblueWhite" )
barplot(1:10, col=PalRedWhiteGreen(10), axes=FALSE, main="PalRedWhiteGreen" )


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ color}