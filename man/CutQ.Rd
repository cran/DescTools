\name{CutQ}
\alias{CutQ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Build Quantile Groups for a Numeric Variable
%%  ~~function to do ... ~~
}
\description{Divides the range of x into some quantiles and codes the values in x according to which interval they fall. 
}
\usage{
CutQ(x, quant = c(.25,.5,.75,1), labels = gettextf("Q\%s", 1:4), na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector which is to be converted to a factor by cutting.
%%     ~~Describe \code{x} here~~
}
  \item{quant}{the breaks for the quantiles, default is quartiles.
%%     ~~Describe \code{x} here~~
}

  \item{labels}{labels for the levels of the resulting category. By default, labels are defined as \code{Q1}, \code{Q2}, \code{Q3} and \code{Q4}. The parameter ist passed to \code{\link{cut}}, so if labels = FALSE, simple integer codes are returned instead of a factor.
%%     ~~Describe \code{labels} here~~
}
  \item{na.rm}{
  logical, indicating whether NA values should be stripped before the computation of the quantile proceeds. Defaults to FALSE. NAs are reported as NA afterwards.
  %%     ~~Describe \code{na.rm} here~~
}

}
\value{A \code{\link{factor}} is returned, unless \code{labels = FALSE} 
which results in an integer vector of level codes.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{cut}}, \code{\link{quantile}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
boxplot(temperature ~ CutQ(temperature, na.rm = TRUE), data=d.pizza)
}

\keyword{ univar }
