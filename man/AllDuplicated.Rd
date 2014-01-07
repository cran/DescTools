\name{AllDuplicated}
\alias{AllDuplicated}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Index Vector of All Values Involved in Ties

%%  ~~function to do ... ~~
}
\description{Returns an index vector of all the values in x which are involved in ties. \cr
So \code{!AllDuplicated} determines all values in a vector x, which
 appear only once (frequency 1).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AllDuplicated(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vector of any type.}
}

\value{logical vector of the same dimension as x.
}

\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{unique}} returns a unique list of all values in x\cr
\code{\link{duplicated}} returns an index vector flagging all elements, which appeared more than once (leaving out the first appearance!)\cr
\code{\link{union}}(A, B) returns a list with the unique values from A and B\cr
\code{\link{intersect}} returns all elements which appear in A and in B\cr
\code{\link{setdiff}}(A, B) returns all elements appearing in A but not in B \cr
\code{\link{setequal}}(A, B) returns TRUE if A contains exactly the same elements as B\cr

%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

x <- c(1:10, 4:6)

AllDuplicated(x)

x[!AllDuplicated(x)]


# union, intersect and friends...
A <- c(sort(sample(1:20, 9)),NA)
B <- c(sort(sample(3:23, 7)),NA)

# all elements from A and B (no duplicates)
union(A, B)
# all elements appearing in A and in B
intersect(A, B)
# elements in A, but not in B
setdiff(A, B)
# elements in B, but not in A
setdiff(B, A)
# Does A contain the same elements as B?
setequal(A, B)


# Find ties in a vector x
x <- sample(letters[1:10], 20, replace=TRUE)
ties <- split(x, x)

# count tied groups
sum(unlist(lapply(ties, function(x) length(x)>1)))

# length of tied groups
lapply(ties, length)[lapply(ties, length)>1]

# by means of table
tab <- table(x)
tab[tab>1]

# count elements involved in ties
sum(tab>1)
# count tied groups
sum(tab[tab>1])


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
