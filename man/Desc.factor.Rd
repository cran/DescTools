\name{Desc.factor}
\alias{Desc.factor}
\alias{Desc.ordered}
\alias{Desc.character}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Describe a factor, an ordered factor or a character vector
%%  ~~function to do ... ~~
}
\description{This reports a rich description of a factor, consisting out of length, number of NAs, number of levels and 
detailed frequencies of all levels. 
The order of the frequency table can be chosen between descending, ascending frequency, labels or levels. 
For ordered factors the order default is \code{"level"}.
Character vectors are treated as unordered factors.
}
\usage{
\method{Desc}{factor}(x, xname = NULL, ord = c("desc", "asc", "name", "level"), 
            maxrows = 12, digits = 3, plotit = FALSE, ...)

\method{Desc}{ordered}(x, xname = NULL, ...)

\method{Desc}{character}(x, xname = NULL, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a single factor, an ordered factor or a character vector to be described.
%%     ~~Describe \code{x} here~~
}
  \item{xname}{the caption of the output.
%%     ~~Describe \code{xname} here~~
}
  \item{ord}{the order for the frequency table. Factors (and character vectors) are by default orderd by their descending 
  frequencies, ordered factors by their natural order.
%%     ~~Describe \code{ord} here~~
}
  \item{maxrows}{numeric. Defines the maximum number of rows to be reported. For factors with lots of levels it is often not interesting to see
  all the levels. Default is 12 (most frequent ones). \cr
  If maxrows < 1 then just as many rows, as the maxrows\% most frequent factors are shown. 
  Say if maxrows is set to 0.8 then as many rows are shown, that the highest cumulative relative frequency is the first going beyond 0.8.
}
   \item{digits}{integer. With how many digits shoud the relative frequencies be formatted? Default is 3.
%%     ~~Describe \code{digits} here~~
}
  \item{plotit}{boolean. Should a plot be created? The factor is plotted with \code{\link{PlotDesc.factor}}. Default is \code{FALSE}.
%%     ~~Describe \code{plotit} here~~
}
  \item{\dots}{further argument to be passed to methods. For ordered factors and character vectors they are passed to \code{Desc.factor}.}
}
\details{
Desc.char converts x to a factor an processes x as factor.\cr

Desc.ordered only changes the standard order for the frequencies to it's intrinsic order, which means order \code{"level"} 
instead of \code{"desc"} in the factor case.
}
\value{
A list containing the following components: 
  \item{length}{the length of the vector}
  \item{n }{the valid entries (NAs are excluded)}
  \item{NAs}{number of NAs}
  \item{levels}{number of levels} 
  \item{unique}{number of unique values. Note that need not be the same, as there might be empty levels. Of course
  unique values can never be less than the number of levels.} 
  \item{dupes}{boolean saying whether there are any duplicates in the vector.} 
  \item{frq}{a data.frame of absolute and relative frequencies given by \code{\link{Freq}}} 
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{Desc}}, \code{\link{PlotDesc}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{

# unordered factor
Desc(d.diamonds$colour)

# ordered factor
Desc(d.diamonds$clarity)

# just the 5 first groups of the factor
Desc(d.diamonds$colour, maxrows = 5)

# just as much rows, as the most frequent 80% of the factors use 
Desc(d.diamonds$colour, maxrows = 0.8)

}

\keyword{ univar }
