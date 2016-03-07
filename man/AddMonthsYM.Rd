\name{AddMonthsYM}
\alias{AddMonthsYM}
\title{Add a Month to a Date
}
\description{The format YYYYMM for dates can sometimes be convenient. Adding a number of months to such a date however is tedious, as the date structure should going to be preserved. Think of e.g. 201201 - 2 months, which would be expected to yield 201111. AddMonthsYM does this job.
}
\usage{
AddMonthsYM(x, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector of integers, representing the dates in the format YYYYMM, to which a number of months has to be added.
}
  \item{n}{the number of months to be added. If n is negative the months will be subtracted.
%%     ~~Describe \code{n} here~~
}
}
\details{All parameters will be recyled if necessary. The therefore used function \code{\link{mapply}} will display a warning, if the longer argument is not a multiple of the length of the shorter one.
}
\value{a vector of class \code{integer} with the same dimension as x, containing the transformed dates.
}
\author{Andri Signorell <andri@signorell.net>, based on code by Roland Rapold
}

\seealso{\code{\link{AddMonths}}; Date functions, like \code{\link{Year}}, \code{\link{Month}}, etc.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
AddMonthsYM(201511, 5)

AddMonthsYM(c(201511, 201302), c(5, 15))
AddMonthsYM(c(201511, 201302), c(5, -4))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ chron }
