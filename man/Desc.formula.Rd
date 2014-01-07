\name{Desc.formula}
\alias{Desc.formula}
\alias{DescFactFact}
\alias{DescNumFact}
\alias{DescNumNum}

\title{Describe Variables by Groups
}
\description{Formula interface for describing data by groups.
}

\usage{
\method{Desc}{formula}(formula, data = parent.frame(), subset = TRUE, ...)
}

\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the data values and rhs the corresponding groups.}  

  \item{data}{an optional matrix or data frame containing the variables in the formula \code{formula}. 
    By default the variables are taken from \code{environment(formula)}.}  
  \item{subset}{an optional vector specifying a subset of observations to be used.}  
  \item{\dots}{further argument to be passed to methods.}
}

\details{The formula interface accepts the formula operators +, :, *, I(), 1 and evaluates any function. 
The left hand side and right hand side of the formula are evaluated the same way.
The variable pairs are processed in dependency of their classes by the functions DescFactFact, DescNumFact and DescNumNum.
}
\value{just printed summary, no value returned
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{

\code{\link{Desc.logical}}, \code{\link{Desc.factor}}, \code{\link{Desc.ordered}}, \code{\link{Desc.integer}},
\code{\link{Desc.numeric}}, \code{\link{Desc.Date}}, \code{\link{Desc.table}}, \code{\link{Desc.data.frame}},  
\code{\link{Desc.list}} 
}
\examples{
# univariate evaluation of temperature and driver
Desc(temperature + driver ~ 1, data=d.pizza, digits=1)

# temperature by driver
Desc(temperature ~ driver, data=d.pizza, digits=1)

# functions are evaluated
Desc(I(temperature^2) + sqrt(temperature) ~ interaction(driver, area), data=d.pizza, digits=1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
