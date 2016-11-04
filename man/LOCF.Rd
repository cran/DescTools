\name{LOCF}
\alias{LOCF}
\alias{LOCF.default}
\alias{LOCF.matrix}
\alias{LOCF.data.frame}
\docType{data}
\title{Last Observation Carried Forward
%%   ~~ data name/kind ... ~~
}
\description{Replace \code{NA}s by the last observed value (aka "Last Observation Carried Forward").
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{
LOCF(x)

\method{LOCF}{default}(x)
\method{LOCF}{data.frame}(x)
\method{LOCF}{matrix}(x)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector, a data.frame or a matrix containing NAs.
%%     ~~Describe \code{x} here~~
}
}
\details{The function will replace all NAs found in a vector with the last earlier value not being NA.
In data.frames each column will be treated as described. }
\value{a vector with the same dimension as x.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Daniel Wollschlaeger <dwoll@psychologie.uni-kiel.de>
%%  ~~who you are~~
}
\seealso{See also the package \pkg{Hmisc} for less coarse imputation functions.}
\examples{
d.frm <- data.frame(
  tag=rep(c("mo", "di", "mi", "do", "fr", "sa", "so"), 4)
, val=rep(c(runif(5), rep(NA,2)), 4) )

d.frm$locf <- LOCF( d.frm$val )
d.frm
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }
