\name{Permn}
\alias{Permn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Determine All Possible Permutations of a Set 
%%  ~~function to do ... ~~
}
\description{ Return the set of permutations for a given set of values. 
The values can be numeric values, characters or factors.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Permn(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector of numeric values or characters. Characters need not be unique.
%%     ~~Describe \code{x} here~~
}
}
\details{
This is an implementation of the Steinhaus-Johnson-Trotter permutation algorithm.
%%  ~~ If necessary, more details than the description above ~~
}
\value{a data.frame with all possible permutations of the values in x. 
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Steinhaus, H. (1964): One hundred problems in elementary mathematics, \emph{New York: Basic Books}, pp. 49-50 

Johnson, Selmer M. (1963): Generation of permutations by adjacent transposition, \emph{Mathematics of Computation} 17 

Trotter, H. F. (August 1962): Algorithm 115: Perm, \emph{Communications of the ACM} 5 (8) 

Sedgewick, R. (1977): Permutation generation methods, \emph{ACM Comput. Surv.} 9 (2) 


%% ~put references to the literature/web site here ~
}
\author{David Kahle <david.kahle@gmail.com>
%%  ~~who you are~~
}

\seealso{\code{\link{combn}}, \code{\link{choose}}, \code{\link{factorial}}, \code{\link{GetAllSubsets}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Permn(letters[2:5])
Permn(2:5)

Permn(c("a","b","c","a"))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
