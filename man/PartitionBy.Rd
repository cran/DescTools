\name{PartitionBy}
\alias{PartitionBy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PartitionBy Evaluates a Function Groupwise 
%%  ~~function to do ... ~~
}
\description{
Split the vector x into partitions and apply the function to each partition separately. Computation restarts for each partition.\cr
The logic is the same as the OLAP functions in SQL, e.g. \code{SUM(x) OVER (PARTITION BY group)}.
}
\usage{
PartitionBy(x, by, FUN, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an atomic object, typically a vector.
%%     ~~Describe \code{x} here~~
}
  \item{by}{list of one or more factors, each of same length as X. The elements are coerced to factors by \code{\link{as.factor}}.
%%     ~~Describe \code{by} here~~
}
  \item{FUN}{Function to apply for each factor level combination.
%%     ~~Describe \code{FUN} here~~
}
  \item{\dots}{optional arguments to FUN: the Note section.
%%     ~~Describe \code{\dots} here~~
}
}
\note{Optional arguments to \code{FUN} supplied by the ... argument are not divided into cells. It is therefore inappropriate for \code{FUN} to expect additional arguments with the same length as X.}
\details{This is more or less the same as the function ave, with the arguments organized a bit different.
%%  ~~ If necessary, more details than the description above ~~
}
\value{a vector with the same length as x containing the groupwise results of FUN.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{ave}}, \code{\link{tapply}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
d.frm <- data.frame(x=rep(1:4,3), v=sample(x=1:3, size=12, replace=TRUE), 
                    g=gl(4,3,labels=letters[1:4]), m=gl(3,4,labels=LETTERS[1:3]))

# SQL-OLAP: sum() over (partition by g)
PartitionBy(d.frm$x, d.frm$g, FUN=sum)
PartitionBy(d.frm$x, FUN=sum)

# more than 1 grouping variables are organized as list as in tapply:
PartitionBy(d.frm$x, list(d.frm$g, d.frm$m), mean)

# count
d.frm$count <- PartitionBy(d.frm$x, d.frm$g, length)

# rank
d.frm$rank <- PartitionBy(d.frm$v, d.frm$g, rank)
d.frm$dense_rank <- PartitionBy(d.frm$v, d.frm$g, DenseRank)
d.frm$rank_desc <- PartitionBy(d.frm$x, d.frm$g, function(x) rank(-x))

# row_number
d.frm$row_number <- PartitionBy(d.frm$v, d.frm$g, function(x) order(x))
d.frm

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar}

