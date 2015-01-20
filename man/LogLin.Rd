\name{LogLin}
\alias{LogLin}
\alias{LogGen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Log Linear Hybrid, Generalized Log
%%  ~~function to do ... ~~
}
\description{Computes the log linear hybrid transformation, resp. generalized log, with the goal to stabilize the variance.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
LogLin(x, a)
LogGen(x, a)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector, matrix or data frame.
%%     ~~Describe \code{x} here~~
}
  \item{a}{cutoff for the linear part of the transformation
%%     ~~Describe \code{a} here~~
}
}
\details{The log linear hybrid transformation function is linear for x <= a and logarithmic for x > a. It is continuously differentiable.
The generalized log and log-linear hybrid transformations were introduced in then context of gen-expression microarray data by Rocke and Durbin (2003).

The function LogLin is currently defined as:\cr
\preformatted{
function (x, a) {
  x[x<=a] <- x[x<=a] / a + log(a) - 1 
  x[x>a] <- log(x[x>a])
  return(x)
} }

and LogGen as:
\preformatted{function (x, a) {
 return(log((x + sqrt(x^2 + a^2)) / 2))
}}

%%  ~~ If necessary, more details than the description above ~~
}
\value{a numeric vector of the same dimensions as x containing the transformed results.

}
\references{
Rocke DM, Durbin B (2003): Approximate variance-stabilizing transformations for gene-expression microarray data, \emph{Bioinformatics}. 22;19(8):966-72.
}
\author{
Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{LogSt}}, \code{\link{log}}
}

\examples{
x <- seq(-10, 50, 0.1 )

plot(LogLin(x, a=5) ~ x, type="l")
grid()
lines(LogLin(x, a=2) ~ x, col="brown")
lines(LogLin(x, a=0.5) ~ x, col="steelblue")

lines(LogGen(x, a=1) ~ x, col="orange")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
