\name{Desc.logical}
\alias{Desc.logical}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Describe a dichotomous variable
%%  ~~function to do ... ~~
}
\description{Description of a dichotomous variable. This can either be a boolean vector, a factor with two levels or a numeric variable
with only two unique values.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{Desc}{logical}(x, main = NULL, digits = 3, 
     conf.level = 0.95, plotit = getOption("plotit", FALSE), ...)
        
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the dichotomous vector to be described. 
%%     ~~Describe \code{x} here~~
}
  \item{main}{the caption for the output.
%%     ~~Describe \code{main} here~~
}
  \item{digits}{integer. With how many digits shoud the relative frequencies be formatted? Default is 3.
%%     ~~Describe \code{digits} here~~
}
  \item{conf.level}{confidence level of the interval.
%%     ~~Describe \code{conf.level} here~~
}
  \item{plotit}{boolean. Should a plot be created? The plot looks like a horizontal bar plot. Default is FALSE.
%%     ~~Describe \code{plotit} here~~
}
  \item{\dots}{the dots are passed to the \code{table} command within \code{Desc.logical}.
%%     ~~Describe \code{conf.level} here~~
}

}
\details{The confidence levels for the relative frequencies are calculated by \code{\link{BinomCI}}, method \code{"Wilson"}
 on a confidence level defined by \code{conf.level}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list containing the following components: 
  \item{length}{the length of the vector (n + NAs).}
  \item{n }{the valid entries (NAs are excluded)}
  \item{NAs}{number of NAs}
  \item{unique}{number of unique values. } 
  \item{frq}{a data.frame of absolute and relative frequencies given by \code{\link{Freq}} if maxlevels > unique values in the vector.} 
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{Desc.factor}}, \code{\link{Desc.integer}}, \code{\link{Desc.numeric}}, \code{\link{Desc.data.frame}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Desc(d.pizza$wine_delivered)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }
