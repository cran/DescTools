\name{Ndec}
\alias{Ndec}

\title{Count Decimal Places of a Number
}

\description{Returns the number of decimal places in the vector x. x must be a character and contain formatted numbers.
}
\usage{
Ndec(x)
}

\arguments{
  \item{x}{is a character vector containing formatted numbers
}

}
\details{
The function is currently defined as:\cr\cr
\preformatted{
Ndec <- function(x) {
  stopifnot(class(x)=="character")
  res <- rep(0, length(x))
  x <- gsub(pattern="[eE].+$", rep="", x=x)
  res[grep("\\.",x)] <- nchar( sub("^.+[.]","",x) )[grep("\\.",x)]
  return(res)
}
}
}
\value{an integer value.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
}
\note{format.info\cr 
[1] ... Breite \cr
[2] ... Anzahl Nachkommastellen  \cr
[3] ... Exponential ja/nein \cr

}


\seealso{\code{\link{format.info}}, \code{\link{Frac}}
}
\examples{

x <- c("0.0000", "0", "159.283", "1.45e+10", "1.4599E+10" )
Ndec(x)

}
\keyword{ arith }






