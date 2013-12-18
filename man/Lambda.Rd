\name{Lambda}
\alias{Lambda}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Goodman Kruskal Lambda
%%  ~~function to do ... ~~
}
\description{Calculate symmetric and asymmetric Goodman Kruskal lambda and their confidence intervals. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Lambda(x, y = NULL, direction = c("symmetric", "row", "column"), conf.level = NA, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector, a matrix or a table.
%%     ~~Describe \code{x} here~~
}
  \item{y}{\code{NULL} (default) or a vector with compatible dimensions to x. If y is provided, \code{table(x, y, ...)} is calculated.
%%     ~~Describe \code{y} here~~
}
 \item{direction}{direction of the calculation. Can be one out of \code{"symmetric"} (default), \code{"row"}, \code{"column"} (abbreviations are allowed).
  If direction is set to \code{"row"} then Lambda(R|C) (column dependent) will be reported.
%%     ~~Describe \code{x} here~~
}
  \item{conf.level}{confidence level for the returned confidence interval, restricted to lie between 0 and 1.
%%     ~~Describe \code{conf.level} here~~
}
  \item{\dots}{further arguments are passed to the function \code{\link{table}}, allowing i.e. to set useNA.
%%     ~~Describe \code{\dots} here~~
}
}
\details{Asymmetric lambda is interpreted as the probable improvement in predicting the column variable Y given knowledge of the row variable X.\cr 
The nondirectional lambda is the average of the two asymmetric lambdas, Lambda(C|R) and Lambda(R|C). 
Lambda (asymmetric and symmetric) has a scale ranging from 0 to 1.
\cr
For x either a matrix or data.frame, or two vectors x and y are expected. In latter case table(x, y) is calculated. 
The function handles NAs the same way the table function does, so tables are by default calculated with omitted NAs. 
Use \code{\link{PairApply}} to calculate pairwise lambdas. Use \code{\link{complete.cases}}, if only the complete cases of a data.frame are to be used. 
%%  ~~ If necessary, more details than the description above ~~
}
\value{a single numeric value if no confidence intervals are requested,\cr 
and otherwise a numeric vector with 3 elements for the estimate, the lower and the upper confidence interval
}

\references{
Agresti, A. (2002) \emph{Categorical Data Analysis}. John Wiley & Sons

Goodman, L. A., and W. H. Kruskal. 1979. \emph{Measures of Association for Cross Classifications}. New
York: Springer-Verlag (contains articles appearing in J. Amer. Statist. Assoc. in 1954,
1959, 1963, 1972).\cr

%% ~put references to the literature/web site here ~
}
\author{Andri Signorell <andri@signorell.net> based on code from Antti Arppe <antti.arppe@helsinki.fi>, Nanina Anderegg (confidence interval symmetric lambda)
}

\seealso{\code{\link{GoodmanKruskalGamma}}, \code{\link{SomersDelta}}, \code{\link{StuartTauC}}, 
\code{\link{GoodmanKruskalTauA}}, \code{\link{KendallTauB}}, \code{\link{cor}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

# example from Goodman Kruskal (1954)

m <- as.table(cbind(c(1768,946,115), c(807,1387,438), c(189,746,288), c(47,53,16)))
dimnames(m) <- list(paste("A", 1:3), paste("B", 1:4))
m

# direction default is "symmetric"
Lambda(m)
Lambda(m, conf.level=0.95)

Lambda(m, direction="row")
Lambda(m, direction="column")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }
