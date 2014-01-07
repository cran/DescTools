\name{PoissonCI}
\alias{PoissonCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Poisson Confidence Interval
%%  ~~function to do ... ~~
}
\description{Computes the confidence intervals of a poisson distributed variable's lambda. Several methods
are implemented, see details.
}
\usage{
PoissonCI(x, conf.level = 0.95, 
          method = c("score", "wald", "agresti-coull", "garwood"), 
          na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector.
%%     ~~Describe \code{x} here~~
}
 \item{conf.level}{confidence level, defaults to 0.95.}
 \item{method}{ character string specifing which method to use; can be one out of 
  \code{"wald"}, \code{"score"}, \code{"agresti-coull"}, \code{"garwood"}.
  Method can be abbreviated. See details. Defaults to \code{"score"}. }
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
}
}
\details{
  The Wald interval is obtained by inverting the acceptance region of the Wald 
  large-sample normal test. 

  The Agresti-Coull interval was proposed by Agresti and Coull (1998) and is a slight
  modification of the Wilson interval. The Agresti-Coull intervals are never shorter 
  than the Wilson intervals.
  

}
\value{
  A vector with 3 elements for estimate, lower confidence intervall and upper for the upper one.  
}
\references{  
  A. Agresti and B.A. Coull (1998) Approximate is better than "exact" for interval 
  estimation of binomial proportions. 
  \emph{American Statistician}, \bold{52}, pp. 119-126.  

  \url{http://www.math.montana.edu/~rjboik/classes/502/ci.pdf}
  \url{http://www.ine.pt/revstat/pdf/rs120203.pdf}

}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{BinomCI}}, \code{\link{MultinomCI}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

x <- rpois(100, lambda=4)

PoissonCI(x, method="wald")
PoissonCI(x, method="garwood")


}
\keyword{ univar }

