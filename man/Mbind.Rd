\name{Mbind}
\alias{Mbind}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Bind k nxm-matrices with the same dimension
%%  ~~function to do ... ~~
}
\description{\code{ftable} is nice to produce flat tables. But it does accept nothing but a n-dim table (resp. array) as argument.
\cr So \code{Mbind} binds two (or n) r x c matrices to one 3-dimensional n x r x c table(array), which can be passed to ftable to produce flat tables.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Mbind(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{a list of 2 or more matrices of the same n x m Dimension.
}
}
\value{a 3dim array of the same class as the input matrices. If there are several classes, the matrices will be coerced following the usual R-rules.
}
\author{ Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{CatTable}}, \code{\link{matrix}}
}
\examples{
m1 <- do.call("rbind", lapply( d.pizza[,c("delivery_min","temperature")], 
  tapply, d.pizza$area, mean, na.rm=TRUE))
names(dimnames(m1)) <- c("vname","area")

m2 <- do.call("rbind", lapply( d.pizza[,c("delivery_min","temperature")], 
  tapply, d.pizza$area, median, na.rm=TRUE))
names(dimnames(m2)) <- c("vname","area")

m3 <- do.call("rbind", lapply( d.pizza[,c("delivery_min","temperature")], 
  tapply, d.pizza$area, sd, na.rm=TRUE))
names(dimnames(m3)) <- c("vname","area")

m <- Mbind(mean=m1, median=m3, sd=m2)
m
class(m)

ftab <- round(ftable(m, col.vars=c("area")),2)
ftab

# two different classes
Mbind( alpha=matrix(letters[1:4], nrow=2), num=matrix(1:4, nrow=2))

# matrices with different dimensions are not allowed, following would raise an error:
# Mbind( matrix(letters[1:4], nrow=2), matrix(1:9, nrow=3))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
