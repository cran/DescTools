\name{Large}
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
Large(x, k = 1, unique = FALSE, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a \code{numeric} vector
%%     ~~Describe \code{x} here~~
}
  \item{k}{an integer >0 defining how many extreme values should be returned. Default is \code{k = 1}. If k > length(x), all values will be returned.
%%     ~~Describe \code{k} here~~
}
  \item{unique}{logical, defining if unique values should be considered or not. If this is set to TRUE a list with the extreme value and its frequency is returned. Default is FALSE.
%%     ~~Describe \code{unique} here~~
}
  \item{na.rm}{logical, indicating whether NA values should be stripped before the computation proceeds. Defaults to FALSE. 
%%     ~~Describe \code{na.rm} here~~
}
}
\details{There are several points discussed about that out there. This implementation uses the function sort(partial), which is'nt the fastest, but a fairly fast one.
%%  ~~ If necessary, more details than the description above ~~
}
\value{either a vector with the k most extreme values, if unique is set to FALSE
or a list, containing the k most extreme values and their respective frequency.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{max}}, \code{\link{HighLow}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

x <- sample(1:10, 1000, rep=TRUE)
Large(x, 3)
Large(x, k=3, unique=TRUE)

# works fine up to 1 M
x <- runif(1000000)
Small(x, 3, unique=TRUE)
Small(x, 3, unique=FALSE)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arith }