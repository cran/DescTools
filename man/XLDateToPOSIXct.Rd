\name{XLDateToPOSIXct}
\alias{XLDateToPOSIXct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert Excel Dates to POSIXct
%%  ~~function to do ... ~~
}
\description{As I repeatedly forgot how to convert Excel dates to POSIX here's the specific function.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
XLDateToPOSIXct(x, tz = "GMT")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the integer vector to be converted.
%%     ~~Describe \code{x} here~~
}
  \item{tz}{a time zone specification to be used for the conversion, if one is required. See \code{\link{as.POSIXct}}.
%%     ~~Describe \code{tz} here~~
}
}
\details{\code{\link{XLGetRange}} will return dates as integer values, because XL stores them as integers. An Excel date can be converted with the (unusual) origin of
\code{as.Date(myDate, origin="1899-12-30")}, which is implemented here.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
return an object of the class POSIXct. Date-times known to be invalid will be returned as NA.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{as.POSIXct}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
XLDateToPOSIXct(41025)
XLDateToPOSIXct(c(41025.23, 41035.52))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ chron }
