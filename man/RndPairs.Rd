\name{RndPairs}
\alias{RndPairs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create Pairs of Correlated Random Numbers
%%  ~~function to do ... ~~
}
\description{Create pairs of correlated random numbers.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RndPairs(n, r, rdist1 = rnorm(n = n, mean = 0, sd = 1), 
         rdist2 = rnorm(n = n, mean = 0, sd = 1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{number of pairs. If length(n) > 1, the length is taken to be the number required.
%%     ~~Describe \code{n} here~~
}
  \item{r}{the correlation between the two sets. 
%%     ~~Describe \code{r} here~~
}
  \item{rdist1, rdist2}{the distribution of the random vector X1 and X2. Default is standard normal distribution.
%%     ~~Describe \code{rdist1} here~~
}
}
\value{a data.frame with 2 columns, X1 and X2 containing the random numbers
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{runif}}, \code{\link{rnorm}}, \code{\link{Random}} and friends
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# produce 100 pairs of a normal distributed random number with a correlation of 0.7
d.frm  <- RndPairs(n=100, r=0.7)

plot(d.frm)
lines(lm(X2 ~ X1,d.frm))

# change the distribution
d.frm  <- RndPairs(n=100, r=0.7, rdist2 = rlnorm(n = 100, meanlog = 1, sdlog = .8))
d.frm  <- RndPairs(n=100, r=0.7, rdist2 = runif(n = 100, -1, 4))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}% __ONLY ONE__ keyword per line
\keyword{IO}% __ONLY ONE__ keyword per line
