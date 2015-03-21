\name{AddMonths}
\alias{AddMonths}
\title{Add a Month to a Date
}
\description{Simple adding a number of months to a date can lead to invalid dates, think of e.g. 2012-01-30 + 1 month. This function offers a ceiling option to make sure that the result is always a valid date. The function would then
yield as.Date("2013-01-31") + 1 month = "2013-02-28". If number \code{n} is negative, the months will be subtracted.  
}
\usage{
AddMonths(x, n, ceiling = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the date to which a number of months has to be added. x can be a date or an integer.
  If it is an integer it will be interpreted as yyyymm.
%%     ~~Describe \code{x} here~~
}
  \item{n}{the number of months to be added. If n is negative the months will be subtracted.
%%     ~~Describe \code{n} here~~
}
  \item{ceiling}{logic. If set to TRUE (default), a ceiling to the last available day of month will be set.
%%     ~~Describe \code{ceiling} here~~
}
}
%% \details{
%%  ~~ If necessary, more details than the description above ~~
%% }
\value{a vector with the same dimension and type as x, containing the transformed dates.
}
\references{
Thanks to Antonio: \url{http://stackoverflow.com/questions/14169620/add-a-month-to-a-date}
}
\author{Andri Signorell <andri@signorell.net>, based on code by Roland Rapold and Antonio
}

\seealso{Date functions, like \code{\link{Year}}, \code{\link{Month}}, etc.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
AddMonths(as.Date("2013-01-01"), 10)
AddMonths(as.Date("2013-01-01"), -5)

AddMonths(201301, 3)
AddMonths(201301, -5)

AddMonths(as.Date("2013-01-31"), 1)
AddMonths(as.Date("2013-01-31"), -2)

AddMonths(as.Date(c("2014-10-12","2013-01-31","2011-12-05")), 3)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ chron }
