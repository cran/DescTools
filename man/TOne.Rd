\name{TOne}
\alias{TOne}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create Table One Describing Baseline Characteristics
%%  ~~function to do ... ~~
}
\description{
Create a table summarizing continuous, categorical and dichotomous variables, optionally stratifyied by one or more variables, while performing adequate statistical tests.
}
\usage{
TOne(x, grp = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a data.frame containing all the variables to be included in the table.
%%     ~~Describe \code{x} here~~
}
  \item{grp}{the grouping variable.
%%     ~~Describe \code{grp} here~~
}
}
\details{There are three types of variables currently implemented: numeric, factor and variables with only two levels.
%%  ~~ If necessary, more details than the description above ~~
}
\value{a character matrix
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

\seealso{ \code{\link{WrdTable}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
TOne(x = d.pizza[, c("temperature","delivery_min","driver","wine_ordered")],
     grp=d.pizza$quality)

\dontrun{
wrd <- GetNewWrd()
WrdTable(TOne(x = d.pizza[, c("temperature","delivery_min","driver","wine_ordered")],
              grp=d.pizza$quality))}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO }
