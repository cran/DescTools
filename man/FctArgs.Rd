\name{FctArgs}
\alias{FctArgs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Retrieve a Functions' Arguments
%%  ~~function to do ... ~~
}
\description{Retrieve a functions' arguments and default values in a list.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
FctArgs(name, sort = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{name of the function.
%%     ~~Describe \code{name} here~~
}
  \item{sort}{logical. Should the function arguments be sorted? Defaults to FALSE.
%%     ~~Describe \code{sort} here~~
}
}
\value{a data.frame with the default in the first columns and with row.names as 
argument names.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{formalArgs}} just returns the name of the arguments, but not their defaults.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
formalArgs(PlotFdist)

# compare:
FctArgs(PlotFdist)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
