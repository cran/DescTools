\name{Zodiac}
\alias{Zodiac}
\title{Calculate the Zodiac of a Date
}
\description{Calculate the sign of zodiac of a date.
}
\usage{
Zodiac(x, lang = c("engl", "deu"), stringsAsFactors = TRUE)
}
\arguments{
  \item{x}{the date to be transformed.}
  \item{lang}{the language of the zodiac names, can be english (default) or german (\code{"deu"}).}
  \item{stringsAsFactors}{logical. If set to TRUE (default) the result will consist of a factor with zodiac signs as levels.
}
}
\details{The really relevant things can sometimes hardly be found. You just discovered such a function... ;-)
}
\value{character vector or factor with the zodiac.
}
\author{Andri Signorell <andri@signorell.net>, based on code from Markus Naepflin
}
\seealso{\code{\link{Year}} and other date functions
}
\examples{
Zodiac(c(Date(1937,7,28), Date(1936,6,1), Date(1966,2,25),
         Date(1964,11,17), Date(1972,4,25)), lang="deu")

d <- sample(seq(as.Date("2015-01-01"), as.Date("2015-12-31"), 1), 120)
z <- Zodiac(d)
Desc(z)
}
\keyword{ chron }
