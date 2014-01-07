\name{WhichFlags}
\alias{WhichFlags}
\alias{WhichFactors}
\alias{WhichNumerics}
\alias{WhichCharacters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the Flags, Factors or Numerics of a data.frame
%%  ~~function to do ... ~~
}
\description{Returns a vector with the names of the columns with the the specific property. \cr
WhichFlags returns the dichotomous columns, WhichFactors the factors, WhichNumerics all the numeric columns.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WhichFlags(d.frm)
WhichFactors(d.frm)
WhichNumerics(d.frm, type = c("all", "numeric", "integer"), excl.flags = FALSE)
WhichCharacters(d.frm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d.frm}{data.frame whose elements should be listed.
%%     ~~Describe \code{d.frm} here~~
}
  \item{type}{type of numeric variables, defaults to \code{"all"}. This is only used by \code{WhichNumerics()}.
}
  \item{excl.flags}{logical. Only used by WhichNumerics. Should flags be excluded or not. Default is FALSE, meaning flags will be included 
  if they are numerics.}
}
\details{
\code{WhichFlags} returns all apparently dichotomous columns, like logicals, integers and numerics with exactly 2 unique
 values or factors with 2 levels.\cr
\code{WhichFactors} returns all factors and ordered factors.\cr
\code{WhichNumerics} returns all numeric columns. Whether integers should be included or not can be defined by \code{type}. Default is \code{"all"}. \cr
\code{WhichCharacters} returns the names of all character vectors in a data.frame.\cr

%%  ~~ If necessary, more details than the description above ~~
}
\value{a character vector with the names of the specific columns
}
\author{Andri Signorell <andri@signorell.net>}

\seealso{\code{\link{class}}, \code{\link{mode}}}

\examples{
data(d.pizza)

WhichFlags(d.pizza)
sapply(d.pizza[,WhichFlags(d.pizza)], class)

WhichFactors(d.pizza)
sapply(d.pizza[,WhichFactors(d.pizza)], class)

WhichNumerics(d.pizza)
WhichNumerics(d.pizza, type="numeric")
WhichNumerics(d.pizza, type="integer")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
