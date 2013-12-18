\name{Frac}
\alias{Frac}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Return the Fractional Part of a Numeric Value 
%%  ~~function to do ... ~~
}
\description{Return the fractional part of a numeric value. 

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Frac(x, dpwr = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the numeric value (or a vector of numerics), whose fractional part is to be calculated.
%%     ~~Describe \code{x} here~~
}
  \item{dpwr}{if \code{dpwr} is not missing, the fractional part will be multiplied by 10^dpwr and returned rounded to integer. Defaults to NA.
%%     ~~Describe \code{d} here~~
}
}
\value{return the fractional part of x.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{Ndec}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- rnorm(5)*100
x
Frac(x)

# multiply by 10^4
Frac(x, dpwr=4)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arith }

