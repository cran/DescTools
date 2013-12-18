\name{RgbToCol}
\alias{RgbToCol}
\alias{RgbToLong}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find the Named R-Color Which Is Nearest to a Given RGB-Color
%%  ~~function to do ... ~~
}
\description{Convert a RGB-color to a named R-Color means looking for a color in the R-palette, which is nearest to the given RGB-color.
The function uses the minimum of squared distance as proximity measure. \cr
RgbToLong converts an RGB-color to a long integer in numeric format.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RgbToCol(col, method = "rgb", metric = "euclidean")
RgbToLong(col)}

%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{col}{the color in rgb code, say a matrix with the red, green and blue code in the rows.  }
    \item{method}{character string specifying the color space to be used. Can be "rgb" (default) or "hsv".}    
    \item{metric}{character string specifying the metric to be used for calculating distances between the colors.
        Available options are "euclidean" (default) and "manhattan". Euclidean distances are root sum-of-squares of 
        differences, and manhattan distances are the sum of absolute differences.}

}
\value{the name of the nearest found R color.
}
\author{Andri Signorell <andri@signorell.net>
}
\seealso{\code{\link{ColToRgb}} and the other conversion functions
}
\examples{
RgbToCol(matrix(c(162,42,42), nrow=3))

RgbToLong(matrix(c(162,42,42), nrow=3))

}
\keyword{ color }
