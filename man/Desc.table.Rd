\name{Desc.table}
\alias{Desc.table}
\alias{Desc.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Describe a 2-dimensional Contingency Table
%%  ~~function to do ... ~~
}
\description{A 2-dimensional table will be described with it's relative frequencies, a short summary containing the total cases, 
the dimensions of the table, chi-square tests and some association measures as phi-coefficient, contingency coefficient and Cramer's V.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{Desc}{table}(x, main = NULL, rfrq = NULL, 
     margins = c(1, 2), plotit = getOption("plotit", FALSE), 
     verbose = c("medium","low","high"), ...)
\method{Desc}{matrix}(x, main = NULL, rfrq = NULL,  
     margins = c(1,2), plotit = getOption("plotit", FALSE), 
     verbose = c("medium","low","high"), ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a 2-dimensional table or matrix
%%     ~~Describe \code{x} here~~
}
  \item{main}{the main caption for the output.
%%     ~~Describe \code{main} here~~
}
  \item{rfrq}{a string with 3 characters, each of them being \code{1} or \code{0}. The first position is interpreted as total 
    percentages, the second as row percentages and the third as column percentages. \cr
    "\code{011}" hence produces a table output with row and column percentages. If set to \code{NULL} \code{rfrq} is defined in 
    dependency of \code{verbose} (\code{verbose = "low"} sets \code{rfrq} to \code{"000"} and else to \code{"111"}, latter meaning all percentages will be reported.)
%%     ~~Describe \code{rfrq} here~~
}
  \item{margins}{a vector, consisting out of 1 and/or 2. Defines the margin sums to be included. 
    Row margins are reported if margins is set to 1. Set it to 2 for column margins and c(1,2) for both. \cr
    Default is \code{NULL} (none). 
}
  \item{plotit}{logical. Should a plot be created? The table will be plotted by \code{\link{PlotDesc.table}}, which creates two mosaicplots. Default is \code{FALSE}. 
%%     ~~Describe \code{plotit} here~~
}
  \item{verbose}{character defining the verbosity of the reported results. One out of \code{c("medium","low","high")}, "medium" being the default. Can be abbreviated.
}
  \item{\dots}{the dots are passed to the function \code{\link{PercTable}}, allowing to set futher arguments like expected values etc.}
}
\details{Note that NAs cannot be handled by this interface, as tables in general come in "as.is", say basically as a matrix without any 
further information about potentially cleared NAs.
%%  ~~ If necessary, more details than the description above ~~
}
\value{no results are returned.
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{
\code{\link{Desc.logical}}, \code{\link{Desc.factor}}, \code{\link{Desc.ordered}}, \code{\link{Desc.integer}},
\code{\link{Desc.numeric}}, \code{\link{Desc.Date}}, \code{\link{Desc.table}}, \code{\link{Desc.data.frame}}, 
\code{\link{Desc.formula}}\cr
\code{\link{PercTable}}
}
\examples{
Desc(table(d.pizza$driver, Weekday(d.pizza$date)), rfrq="100", plotit=TRUE)

tab <- as.matrix(read.table(text="
  549 212  54
   93 124  54
  233  78  33
  119  42  13
  225  41  46
  455  12   7
  402 132 153"
))

tab

# taciturn 
Desc(tab, verbose="low")  

# talkative  
Desc(tab, verbose="high", expected=TRUE, res=TRUE)
}
\keyword{ multivariate }
