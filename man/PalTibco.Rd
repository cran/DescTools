\name{PalTibco}
\alias{PalTibco}
\alias{PalRedToBlack}
\alias{PalRedBlackGreen}
\alias{PalSteeblueWhite}
\alias{PalRedWhiteGreen}
\alias{PalHelsana}
\alias{hblue}
\alias{hred}

\title{Some More Color Palettes
}

\description{Defines some more color palettes.

}
\usage{
PalTibco()
PalHelsana()
PalRedToBlack(n = 100)
PalRedBlackGreen(n = 100)
PalSteeblueWhite(n = 100)
PalRedWhiteGreen(n = 100)

hblue
hred

}

\arguments{
  \item{n}{the number of colors (= 1) to be in the palette.

}
}
\details{
hblue and hred are 2 constants, pointing to the red and blue from the palette PalHelsana.
}

\value{
A vector of color values in hexform. 
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{SetAlpha}}, \code{\link{HexToCol}}, \code{\link{ColToHex}}
}
\examples{

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

\keyword{ color}
