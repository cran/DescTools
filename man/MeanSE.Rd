\name{MeanSE}
\alias{MeanSE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Standard error of mean
%%  ~~function to do ... ~~
}
\description{
Calculates the standard error of mean. If x is a matrix or a data frame, a vector of the standard error of the columns is returned. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MeanSE(x, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.
%%     ~~Describe \code{x} here~~
}
  \item{na.rm}{logical. Should missing values be removed? Defaults to FALSE.
%%     ~~Describe \code{na.rm} here~~
}
}
\details{MeanSE calculates the standard error of the mean defined as sd(x)/sqrt(length(x)).
}

\value{For a data frame or for a matrix, a named vector with the appropriate method being applied column by column.
}
\author{Andri Signorell <andri@signorell.net>}

\seealso{\code{\link{MeanCI}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(d.pizza)

MeanSE(d.pizza$price, na.rm=TRUE)

# evaluate data.frame
sapply(d.pizza[,1:4], MeanSE, na.rm=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }

