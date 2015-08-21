\name{SetRowNames}
\alias{SetRowNames}
\alias{SetColNames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Set the Row Names and Column Names in an Object
%%  ~~function to do ... ~~
}
\description{This is a convenience function that sets the row names and column names on an object and returns the object. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SetRowNames(object = nm, nm)
SetColNames(object = nm, nm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object for which a names attribute will be meaningful
%%     ~~Describe \code{object} here~~
}
  \item{nm}{a character vector of names to assign to the object
%%     ~~Describe \code{nm} here~~
}
}
\value{An object of the same sort as object with the new names assigned.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>\cr
but it is merely a copy of \code{\link{setNames}} extended for rows and columns
%%  ~~who you are~~
}

\seealso{\code{\link{setNames}} 
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
tab <- table(d.pizza$driver, d.pizza$wine_delivered)
SetRowNames(BinomCI(tab[,1], rowSums(tab)), rownames(tab))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ list }



