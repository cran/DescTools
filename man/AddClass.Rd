\name{AddClass}
\alias{AddClass}
\alias{RemoveClass}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add and Remove Classes From an Object
%%  ~~function to do ... ~~
}
\description{A simple wrapper for adding and remove classes from an object.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AddClass(x, class, after = 0)

RemoveClass(x, class)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a \code{R} object
%%     ~~Describe \code{x} here~~
}
  \item{class}{a character vector naming classes.
%%     ~~Describe \code{class} here~~
}
  \item{after}{a subscript, after which the class(es) are to be appended.
%%     ~~Describe \code{after} here~~
}
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{class}}, \code{\link{append}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- letters[1:5]
x <- AddClass(x, "special")
}

