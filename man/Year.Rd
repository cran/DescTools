\name{Year}
\alias{Year}
\alias{Quarter}
\alias{Month}
\alias{Week}
\alias{Day}
\alias{Day<-}
\alias{Weekday}
\alias{Yearday}
\alias{Yearmonth}
\alias{IsWeekend}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{Part of Date
%%  ~~function to do ... ~~
}
\description{Returns the specific part of a given date x.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Year(x)
Quarter(x)
Month(x, format = c("num", "abbr", "full"), lang = c("local", "engl"), 
      stringsAsFactors = TRUE)
Week(x)
Day(x)
Weekday(x, format = c("num", "abbr", "full"), lang = c("local", "engl"), 
        stringsAsFactors = TRUE)
Yearday(x)
Yearmonth(x)

Day(x) <- value

IsWeekend(x)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the date to be evaluated.
%%     ~~Describe \code{x} here~~
}
  \item{format}{defines how the month or the weekday are to be formatted. Defaults to \kbd{"num"}  and can be abbreviated. Is ignored for other functions.
%%     ~~Describe \code{format} here~~
}
  \item{value}{new value
%%     ~~Describe \code{value} here~~
}
  \item{lang}{the language for the months and daynames. This can be either the current locale (default) or english.
%%     ~~Describe \code{value} here~~
}
  \item{stringsAsFactors}{logical. Defines if the result should be coerced to a factor, using the local definitions as levels.
  The result would be an ordered factor. Default is TRUE.
}
}

\details{These functions are only convenience wrappers for \code{format()} and its strange codes...\cr
Based on the requested time component, the output is as follows:\cr\cr
\kbd{Year} returns the year of the input date in yyyy format.\cr 
\kbd{Quarter} returns the quarter of the year (1 to 4) for the input date. \cr
\kbd{Month} returns the month of the year (1 to 12) for the input date. \cr
\kbd{Week} returns the week of the year for the input date (0 to 53), as defined in ISO8601. \cr
\kbd{Weekday} returns the week day of the input date. (1 - Monday, 2 - Tuesday, ... 7 - Sunday). (Names and abbreviations in the current locale!)\cr
\kbd{Yearday} returns the day of the year numbering (1 to 366). \cr
\kbd{Day} returns the day of the month (1 to 31). \cr
\kbd{Yearmonth} returns the yearmonth representation (YYYYMM) of a date as longinteger. \cr

\kbd{IsWeekend} returns TRUE, if the date x falls on a weekend. \cr

The day can not only be extracted, but as well defined. See examples.
%%  ~~ If necessary, more details than the description above ~~
}
\value{a vector of the same dimension as x, consisting of either numeric values or characters depending on the function used.
}
\author{
Andri Signorell <andri@signorell.net>
}

\seealso{
\code{\link{strptime}}, \code{\link{DateTimeClasses}}, \code{\link{as.POSIXlt}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- Sys.Date()

Year(x)
Quarter(x)

Month(x)
Month(x, format = "abb", lang="engl")
Month(x, format = "abb", lang="local")
Month(x, format = "full", lang="engl")
Month(x, format = "full", lang="local")

Week(x)

Day(x)
Day(x) <- 20
x

Weekday(x)
Weekday(x, format = "abb", lang="engl")
Weekday(x, format = "abb", lang="local")
Weekday(x, format = "full", lang="engl")
Weekday(x, format = "full", lang="local")

Yearday(x)

IsWeekend(x)

# let's generate a time sequence by weeks
Month(seq(from=as.Date(Sys.Date()), to=Sys.Date()+150, by="weeks"), format="a")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{chron}
