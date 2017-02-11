\name{Date}
\alias{Date}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a Date From Numeric Representation
%%  ~~function to do ... ~~
}
\description{Create a date out of either year, month and day supplied by single values or out of one
single numeric valu in the form yyyymmdd.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Date(year, month = NA, day = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{year, month, day}{numerical values to specify a day. If month and day are omitted
  the function tries to interpret year as yearmonthday (yyyymmdd) long integer value.
}
}
\details{
All arguments are recycled if necessary.\cr

The function is a convenience wrapper for ISOdate, which yields a datetime object, which again is
an overkill, if only dates are needed.
}

\value{a vector with the same length as the input vector of class \code{"Date"}.
}
\author{Andri Signorell <andri@signorell.net>
}
\seealso{\code{\link{ISOdate}}
}
\examples{
Date(2011, 3, 1:15)

Date(20120305:20120315)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ chron }
