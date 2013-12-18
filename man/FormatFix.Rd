\name{FormatFix}
\alias{FormatFix}
\title{Format to a fixed format representation}
\description{
  \code{FormatFix} formats to fixed point number format.
  It writes x with sign (" " or "-") and \code{"before"} decimals before the "." and
  with \code{"after"} decimals after the ".". If \code{"after"} == 0 then the "." will
	be omitted.

	There will always be at least one decimal digit before the "."

	If \code{"before"} is too small to represent \code{x} then if
	\code{extend == TRUE}, the string will be extended, else a string
	consisting of "*" of length \code{before + after} will be given.

	If \code{abs(x) >= 10^8} values very near \code{10^k} cannot be represented
	exactly, so the normal \code{format} will be used.

	Names are retained.
}
\usage{
FormatFix(x, after, before = 2,extend = TRUE)
}
\arguments{
  \item{x}{the number to be represented.}
  \item{after}{the number of decimals after ".".}
  \item{before}{the minimum number of decimals before ".".}
  \item{extend}{extend string if necessary.}
}
\value{
  The string representing the fixed point format of \code{x}.
}

\note{ This function was previously published as \code{formatFix()} in the  \pkg{cwhmisc} package and has been integrated here without logical changes.
}

\examples{
xxbig <- c(1.2e9,3.51e23,6.72e120,NaN)
xx    <- c(0.001,92,exp(1),1000*pi)
FormatFix(c(-rev(xxbig),-rev(xx),0,NA,xx,xxbig),0,3)
#> [1] "    NaN" "-7e+120" " -4e+23" " -1e+09" "  -3142" "     -3" "    -92"
#> [8] "     -0" "      0" "     NA" "      0" "     92" "      3" "   3142"
#> [15] "  1e+09" "  4e+23" " 7e+120" "    NaN"
FormatFix(c(-rev(xxbig),-rev(xx),0,NA,xx,xxbig),0,3,FALSE)
#> [1] "NaN" "***" "***" "***" "***" " -3" "-92" " -0" "  0" " NA" "  0" " 92"
#> [13] "  3" "***" "***" "***" "***" "NaN"
FormatFix(c(-rev(xxbig),-rev(xx),0,NA,xx,xxbig),6,3)
#> [1] "         NaN" "  -6.72e+120" "   -3.51e+23" "    -1.2e+09" "-3141.592654"
#> [6] "   -2.718282" "  -92.000000" "   -0.001000" "    0.000000" "          NA"
#> [11] "    0.001000" "   92.000000" "    2.718282" " 3141.592654" "     1.2e+09"
#> [16] "    3.51e+23" "   6.72e+120" "         NaN"
FormatFix(c(-rev(xxbig),-rev(xx),0,NA,xx,xxbig),6,3,FALSE)
#> [1] "       NaN" "-6.72e+120" " -3.51e+23" "  -1.2e+09" "**********"
#> [6] " -2.718282" "-92.000000" " -0.001000" "  0.000000" "        NA"
#> [11] "  0.001000" " 92.000000" "  2.718282" "**********" "   1.2e+09"
#> [16] "  3.51e+23" " 6.72e+120" "       NaN"
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>
}
\keyword{print}
