\name{PlotRCol}
\alias{PlotRCol}
\alias{PlotPar}
\alias{PlotMar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Information plots
%%  ~~function to do ... ~~
}
\description{The function \code{PlotPar()} plots the typically used parameters and their values. \cr
\code{PlotRCol()} plots the R-colors in a dense manner.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotRCol(ord = c("hsv", "default"), label = c("text", "hex", "dec"),
         mdim = c(38, 12))
PlotPar()
PlotMar()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ord}{the order of the colors, can be either defined by hsv-value or by the R internal color-number
%%     ~~Describe \code{ord} here~~
}
  \item{label}{label for the colors, can be the colorname (text), the hex-code (#RRGGBB) or the decimal RGB-number
%%     ~~Describe \code{label} here~~
}
  \item{mdim}{the dimension of the color matrix. Default is 38 rows and 12 columns.}
}
\details{\code{PlotMar()} should plot the margins, but waits for its implementation...
%%  ~~ If necessary, more details than the description above ~~
}
\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{par}}, \code{\link{colors}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
PlotPar()

PlotRCol(ord="hsv")
PlotRCol(label="hex")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }

