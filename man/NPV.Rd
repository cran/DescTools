\name{NPV}
\alias{NPV}
\alias{IRR}
\alias{OPR}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{One Period Returns, Net Present Value and Internal Rate of Return
%%  ~~function to do ... ~~
}
\description{Calculate the one period returns, the net present value (NPV) and internal rate of return (IRR) of a sequence of payments.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
OPR(K, D = NULL, log = FALSE)
NPV(i, cf, t = seq(along = cf) - 1)
IRR(cf, t = seq(along = cf) - 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{i}{the interest rate
%%     ~~Describe \code{i} here~~
}
  \item{cf}{numeric vector with the payments
%%     ~~Describe \code{cf} here~~
}
  \item{t}{periods
%%     ~~Describe \code{t} here~~
}
  \item{K}{the capital at time t}
  \item{D}{dividend at time t }
  \item{log}{logical, determining if the simple returns (default) or log returns are to be calculated.}
  
}
\value{a numeric value
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\details{The one period returns are calculated as 
\deqn{r_t = \frac{D_t+K_t-K_t-1}{K_t-1}}
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{Gmean}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
cf <- c(-900, -250+450-90, 460-100, 500-120, 550-140)
IRR(cf)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arith }
