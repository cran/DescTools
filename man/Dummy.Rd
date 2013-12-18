\name{Dummy}
\alias{Dummy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generate Dummy Codes for a Factor 
%%  ~~function to do ... ~~
}
\description{Generates a matrix of dummy codes (class indicators) for a given factor. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Dummy(x, method = c("treatment", "sum", "helmert", "poly", "full"), base = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{factor or vector of classes for cases.
%%     ~~Describe \code{x} here~~
}
  \item{method}{defines the method of the contrasts being formed. Can be one 
  out of \code{"treatment"}, \code{"sum"}, \code{"helmert"}, \code{"poly"}, \code{"full"}, 
  whereas \code{"treatment"} is the default one. Abbreviations are accepted.\cr
  The option \code{"full"} returns a full set of class indicators, say a dummy factor for EACH level of x. 
  Note that this would be redundant for lm and friends!
%%     ~~Describe \code{method} here~~
}
  \item{base}{an integer specifying which group is considered the baseline group. 
%%     ~~Describe \code{base} here~~
}
}
\value{a matrix with the dummy codes. 
The rows correspond to the number of elements in x and the columns to it's levels.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Venables, W N and Ripley, B D (2002): \emph{Modern Applied Statistics with S}. Fourth edition. Springer. 
%% ~put references to the literature/web site here ~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{model.frame}},  \code{\link{contrasts}}, \code{\link[nnet]{class.ind}} in the package \pkg{nnet} 
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

x <- c("red","blue","green","blue","green","red","red","blue") 
Dummy(x) 
Dummy(x, base = 2)

Dummy(x, method = "sum")


y <- c("Max","Max","Max","Max","Max","Bill","Bill","Bill")

Dummy(y) 
Dummy(y, base = "Max")

Dummy(y, base = "Max", method="full")


# "Undummy" (revert the dummy coding) 
m <- Dummy(y, method="full")
m
z <- apply(m, 1, function(x) colnames(m)[x==1])
z
identical(y, as.vector(z))

m <- Dummy(y)
m
z <- apply(m, 1, function(x) ifelse(sum(x)==0, attr(m,"base"), colnames(m)[x==1]))
z

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
