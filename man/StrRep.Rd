\name{StrRep}
\alias{StrRep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Repeat the Elements of a Character Vector
%%  ~~function to do ... ~~
}
\description{Repeat the character strings in a character vector a given number of times (i.e., concatenate the respective numbers of copies of the strings).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StrRep(x, times, sep = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a character vector, or an object which can be coerced to a character vector using as.character.
%%     ~~Describe \code{x} here~~
}
  \item{times}{an integer vector giving the (non-negative) numbers of times to repeat the respective elements of x.
%%     ~~Describe \code{times} here~~
}
  \item{sep}{a character string to separate the terms. Not \code{\link{NA_character_}}.
%%     ~~Describe \code{sep} here~~
}
}
\details{The elements of x and times will be recycled as necessary (if one has no elements, and empty character vector is returned). Missing elements in x or times result in missing elements of the return value.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A character vector with the elements of the given character vector repeated the given numbers of times.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{strrep}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
StrRep(x="wiki", times=1:3, sep=c("", "-"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{character}
