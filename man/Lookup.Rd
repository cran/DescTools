\name{Lookup}
\alias{Lookup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Look Up a Value In a Reference Vector
%%  ~~function to do ... ~~
}
\description{\code{Lookup} searches the positions of (first) matches of the argument x in its second argument idx and returns the according values from ret.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Lookup(x, ref, val)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vector: the values to be matched. Long vectors are supported.
%%     ~~Describe \code{x} here~~
}
  \item{ref}{vector: the values to be matched. Long vectors are supported.
%%     ~~Describe \code{idx} here~~
}
  \item{val}{vector: the values to be returned.
%%     ~~Describe \code{ret} here~~
}
}
\details{This function is wrapping the function \code{\link{match}} allowing to pass a vector from which the found elements should be returned. This is often used in a code lookup situation and might be simpler than using \code{\link{match}} or \code{\link{merge}}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector of the same length as \code{x}
giving the elements in \code{ret} at the position in \code{idx} of the first match if there is a match. If there's no match \code{NA} will be returned.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{match}}, \code{\link{merge}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

x <- c("a", "h", "b")
z <- LETTERS
Lookup(x, letters, z)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip}

