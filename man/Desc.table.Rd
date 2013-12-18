\name{Desc.table}
\alias{Desc.table}
\alias{Desc.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Describe a contingency table
%%  ~~function to do ... ~~
}
\description{A table will be described with it's relative frequencies, a short summary containing the total cases, 
the dimensions of the table, chi-square tests and phi-coefficient, contingency coefficient and Cramer's V as association measures.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{Desc}{table}(x, xname = NULL, rfrq = NULL, 
           margins = c(1, 2), plotit = FALSE, verb = c("med","lo","hi"), ...)
\method{Desc}{matrix}(x, xname = NULL, rfrq = NULL,  
            margins = c(1,2), plotit = FALSE, verb = c("med","lo","hi"), ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a 2-dimensional table or a matrix
%%     ~~Describe \code{x} here~~
}
  \item{xname}{the caption for the output.
%%     ~~Describe \code{xname} here~~
}
  \item{rfrq}{a string with 3 characters, each of them being 1 or 0. The first position means total 
    percentages, the second means row percentages and the third column percentages. \cr
    "\code{011}" produces a table output with row and column percentages. If set to NULL rfrq is defined in 
    dependency of verb (verb = "lo" sets rfrq to "000" and else to "111", latter meaning all percentages will be reported.)
%%     ~~Describe \code{rfrq} here~~
}
  \item{margins}{a vector, consisting out of 1 and/or 2. Defines the margin sums to be included. 
    Row margins are reported if margins is set to 1. Set it to 2 for column margins and c(1,2) for both. \cr
    Default is \code{NULL} (none). 
}
  \item{plotit}{logical. Should a plot be created? Default is \code{FALSE}.\cr
    The table would be plotted by \code{\link{PlotDesc.table}}.
%%     ~~Describe \code{plotit} here~~
}
  \item{verb}{character defining the verbosity of the reported results. One out of c("med","lo","hi"), "med" being the default.
}
  \item{\dots}{further argument to be passed to methods.}
}
\details{Note that NAs cannot be handled by this interface, as tables in general come in "as.is", say basically as a matrix without any 
further information about potentially cleared NAs.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
no results are returned.
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{
\code{\link{Desc.logical}}, \code{\link{Desc.factor}}, \code{\link{Desc.ordered}}, \code{\link{Desc.integer}},
\code{\link{Desc.numeric}}, \code{\link{Desc.Date}}, \code{\link{Desc.table}}, \code{\link{Desc.data.frame}}, 
\code{\link{Desc.formula}} 
}
\examples{

Desc(table(d.pizza$driver, Weekday(d.pizza$date)), rfrq="100", plotit=TRUE)


tab <- t(rbind(c(549,212,54),
           c(93,124,54),
           c(233,78,33),
           c(119,42,13),
           c(225,41,46),
           c(455,12,7),
           c(402,132,153)
     ))

# taciturn 
Desc(tab, verb="lo")  

# talkative  
Desc(tab, verb="hi", expected=TRUE, res=TRUE)
}
\keyword{ multivariate }
