\name{Extremes}
\alias{Large}
\alias{Small}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Kth Smallest/Largest Values
%%  ~~function to do ... ~~
}
\description{This function returns the kth smallest, resp. largest values from a vector x.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Small(x, k = 5, unique = FALSE, na.last = NA)
Large(x, k = 5, unique = FALSE, na.last = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a \code{numeric} vector
%%     ~~Describe \code{x} here~~
}
  \item{k}{an integer >0 defining how many extreme values should be returned. Default is \code{k = 5}. If \code{k > length(x)}, all values will be returned.
%%     ~~Describe \code{k} here~~
}
  \item{unique}{logical, defining if unique values should be considered or not. If this is set to \code{TRUE}, a list with the \code{k} extreme values and their frequencies is returned. Default is \code{FALSE} (as unique is a rather expensive function).
%%     ~~Describe \code{unique} here~~
}
\item{na.last}{for controlling the treatment of \code{NA}s.
    If \code{TRUE}, missing values in the data are put last; if
    \code{FALSE}, they are put first; if \code{NA}, they are removed.}

}
\details{There are several points of this problem discussed out there. This implementation is based on effective C++ code, which is quite fast.
%%  ~~ If necessary, more details than the description above ~~
}
\value{either a vector with the k most extreme values, if \code{unique} is set to \code{FALSE}
or a list, containing the k most extreme values and their respective frequency.
}
\author{Andri Signorell <andri@signorell.net>\cr
C++ parts by Nathan Russell and Romain Francois
}

\references{\url{http://stackoverflow.com/questions/36993935/find-the-largest-n-unique-values-and-their-frequencies-in-r-and-rcpp/}

\url{http://gallery.rcpp.org/articles/top-elements-from-vectors-using-priority-queue/}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{max}}, \code{\link{max}}, \code{\link{HighLow}}, \code{\link{sort}}, \code{\link{rank}}
}
\examples{
x <- sample(1:10, 1000, rep=TRUE)
Large(x, 3)
Large(x, k=3, unique=TRUE)

# works fine up to x ~ 1e6
x <- runif(1000000)
Small(x, 3, unique=TRUE)
Small(x, 3, unique=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arith }
