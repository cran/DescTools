\name{HexToRgb}
\alias{HexToRgb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert a Hexstring Color to a Matrix With Three Red/Green/Blue Rows
%%  ~~function to do ... ~~
}
\description{Converts a hexstring color to matrix with 3 red/green/blue rows.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
HexToRgb(hex)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hex}{a color or a vector of colors specified as hexadecimal string of the form "#rrggbb" or "#rrggbbaa"}
%%     ~~Describe \code{hex} here~~
}

\value{a matrix with 3 rows.
}
\author{Andri Signorell <andri@signorell.net>
}
\seealso{\code{\link{HexToCol}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
HexToRgb(c("#ADD8E6", "#FA1572"))
}
\keyword{ color }
