\name{BinomDiffCI}
\alias{BinomDiffCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Confidence Interval for a Difference of Binomials
%%  ~~function to do ... ~~
}
\description{Calculate confidence interval for a risk difference. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BinomDiffCI(x1, n1, x2, n2, conf.level = 0.95, 
            method = c("wald", "waldcor", "ac", "exact", "newcombe", 
                       "newcombecor", "fm", "ha"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{ number of successes for the first group. }
  \item{n1}{ number of trials for the first group. }
  \item{x2}{ number of successes for the second group. }
  \item{n2}{ number of trials for the second group. }
  \item{conf.level}{ confidence level, defaults to 0.95. }
  \item{method}{ one out of "wald", "waldcor", "ac", "exact", "newcombe", "newcombecor", "fm", "ha".
%%     ~~Describe \code{method} here~~
}
}
\details{Still to follow. Methods "newcombecor", "fm", "ha" are not implemented yet.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  A vector with 3 elements for estimate, lower confidence intervall and upper for the upper one.  
 }
\references{
Fagerland M W, Lydersen S and Laake P (2011) Recommended confidence intervals for two independent binomial proportions, Statistical Methods in Medical Research 0(0) 1-31

\url{http://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/viewer.htm#statug_freq_a0000000642.htm#statug.freq.freqbincp}
%% ~put references to the literature/web site here ~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{BinomCI}},  \code{\link{MultinomCI}},  \code{\link{binom.test}},  
\code{\link{prop.test}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
BinomDiffCI(14, 70, 32, 80, method="wald")
BinomDiffCI(14, 70, 32, 80, method="waldcor")
BinomDiffCI(14, 70, 32, 80, method="ac")
BinomDiffCI(14, 70, 32, 80, method="newcombe")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{category}
