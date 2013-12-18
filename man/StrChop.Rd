\name{StrChop}
\alias{StrChop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Split a String in a Number of Pieces With Fixed Length
%%  ~~function to do ... ~~
}
\description{Split a string in a number of pieces with fixed length
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StrChop(x, len)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the string to be cut in pieces.
%%     ~~Describe \code{x} here~~
}
  \item{len}{a vector with the lengths of the pieces.
%%     ~~Describe \code{len} here~~
}
}
\details{If length is going over the end of the string the last part will be returned.
%%  ~~ If necessary, more details than the description above ~~
}
\value{a vector with the parts of the string.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
}


\seealso{\code{\link{FixToTab}}
}
\examples{

x <- paste(letters, collapse="")
StrChop(x=x, len = c(3,5,2))


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ character }
