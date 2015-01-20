\name{Exec}
\alias{Exec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Execute a Command Given As String
%%  ~~function to do ... ~~
}
\description{Execute a piece of code. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Exec(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the code to be executed
%%     ~~Describe \code{x} here~~
}
}
\details{This is just a wrapper to eval and parse, but easier to remember...
%%  ~~ If necessary, more details than the description above ~~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{eval}}, \code{\link{parse}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
A <- "This"
B <- "is"
C <- "a text"
for(s in LETTERS[1:3])
  Exec(gettextf("print(\%s)", s))

}
\keyword{ manip }
