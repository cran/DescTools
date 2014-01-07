\name{LOCF}
\alias{LOCF}
\docType{data}
\title{Last Observation Carried Forward
%%   ~~ data name/kind ... ~~
}
\description{Replace NAs by the last observed value (aka "Last Observation Carried Forward").
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{
LOCF(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{x is a vector containing NAs.
%%     ~~Describe \code{x} here~~
}
}
\value{a vector with the same dimension as x.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
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
