\name{\%like\%}
\alias{\%like\%}
\alias{\%like any\%}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Like operator
%%  ~~function to do ... ~~
}
\description{The like operator is a simple wrapper for \code{\link{grepl}}, whose complexity is hard to crack for R-newbies.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
x \%like\% pattern

x \%like any\% pattern
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector, typically of character or factor type
%%     ~~Describe \code{x} here~~
}
  \item{pattern}{simple character string to be matched in the given character vector.
%%     ~~Describe \code{pattern} here~~
}
}
\details{Follows the logic of simple SQL or basic commands.
%%  ~~ If necessary, more details than the description above ~~
}
\value{ a vector (numeric, character, factor), matching the mode of \code{x}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{match}}, \code{\link{pmatch}}, \code{\link{grep}}, \code{\link{\%[]\%}}, \code{\link{\%overlaps\%}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# find names ending on "or"
names(d.pizza) \%like\% "\%or"

# find names starting with "d"
names(d.pizza) \%like\% "d\%"

# ... containing er?
names(d.pizza) \%like\% "\%er\%"

# the positions on the vector
which(names(d.pizza) \%like\% "\%er\%")

# what do they look like?
names(d.pizza)[names(d.pizza) \%like\% "\%er\%"]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
