\name{ColPicker}
\alias{ColPicker}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot All Named R Colors
%%  ~~function to do ... ~~
}
\description{\code{ColPicker()} plots the R-colors in a dense manner and allows to collect some colors by using a locator. This can be helpful when some colors should be put together to a palette.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ColPicker(locator = TRUE, ord = c("hsv", "default"), label = c("text", "hex", "dec"),
          mdim = c(38, 12), newwin = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{locator}{logical, defines if the colors can be selected by mouseclick. Default is TRUE.}
  \item{ord}{the order of the colors, can be either defined by hsv-value or by the R internal color-number. Default is the latter.
%%     ~~Describe \code{ord} here~~
}
  \item{label}{label for the colors, can be the colorname (text), the hex-code (#RRGGBB) or the decimal RGB-number
%%     ~~Describe \code{label} here~~
}
  \item{mdim}{the dimension of the color matrix. Default is 38 rows and 12 columns.}
  \item{newwin}{logical, defining if a new graphic device should be used. Default is \code{FALSE}.}
}
\details{
The function plots all the colors but leaves out the grey scales \code{grey()} and \code{gray()}.
}

\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{ColorDlg}}, \code{\link{par}}, \code{\link{colors}}, \code{\link{PlotPch}}
}
\examples{
ColPicker(locator=FALSE, ord="hsv")
ColPicker(locator=FALSE, label="hex")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }

