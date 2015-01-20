\name{OddsRatio}
\alias{OddsRatio}


\title{Odds Ratio Estimation and Confidence Intervals}

\description{
  Calculates odds ratio by unconditional maximum likelihood estimation (wald),
  conditional maximum likelihood estimation (mle) and median-unbiased estimation (midp). 
  Confidence intervals are calculated using normal approximation (wald) and exact methods
  (midp, mle).  
}

\usage{
OddsRatio(x, y = NULL, conf.level = NA, method = c("wald", "mle", "midp"),
          interval = c(0, 1000), ...)
}

\arguments{
  
  \item{x}{a numeric vector or a 2x2 numeric matrix, resp. table.
%%     ~~Describe \code{x} here~~
}
  \item{y}{NULL (default) or a vector with compatible dimensions to \code{x}. If y is provided, \code{table(x, y, \dots)} will be calculated.
%%     ~~Describe \code{y} here~~
}
  \item{method}{
    method for calculating odds ratio and confidence interval. Can be one out of
    "wald", "mle", "midp". Default is "wald" (not because it is the best, but 
    because it is the most commonly used.)
  }
  \item{conf.level}{confidence level. Default is NA, meaning no confidence intervals will be
  reported.}
  \item{interval}{
    interval for the \code{\link{uniroot}} that finds the
    odds ratio median-unbiased estimate and mid-p exact confidence
    interval.
  }
   \item{\dots}{further arguments are passed to the function \code{\link{table}}, allowing i.e. to set useNA. This refers only to the vector interface.
%%     ~~Describe \code{\dots} here~~
}

}

\details{

  If a 2x2 table is provided the following table structure is preferred:
  \preformatted{
                    disease=0   disease=1
    exposed=0 (ref)    n00         n01
    exposed=1          n10         n11	
  }
  however, for odds ratios from 2x2 tables, the following table is
  equivalent:
  \preformatted{
                    disease=1   disease=0
    exposed=1          n11         n10
    exposed=0          n01         n00	
  }
  If the table to be  provided to this function is not in the
  preferred form, just use the function \code{\link{Rev}()} to "reverse" the table rows,
  -columns, or both. 
  
  If a data.frame is provided the odds ratios are calculated pairwise and returned as numeric square matrix 
  with the dimension of ncol(data.frame).
  
%  If you are providing categorical variables (factors
%  or character vectors), the first level of the "exposure" variable is
%  treated as the reference. However, you can set the reference of a
%  factor using the \code{\link[stats]{relevel}} function.

%  Likewise, each row of the rx2 table is compared to the exposure
%  reference level and test of independence two-sided p values are
%  calculated using mid-p exact, Fisher's Exact, Monte Carlo simulation,
%  and the chi-square test. 
In case of zero entries, 0.5 will be added to the table.
}


\value{
 If conf.level is not NA then the result will be
  a vector with 3 elements for estimate, lower confidence intervall and upper for the upper one.
  Else the odds ratio will be reported as a single value. 
}

\references{
  Kenneth J. Rothman and Sander Greenland (1998): \emph{Modern Epidemiology},
  Lippincott-Raven Publishers
  
  Kenneth J. Rothman (2002): \emph{Epidemiology: An Introduction}, Oxford
  University Press
  
  Nicolas P. Jewell (2004): \emph{Statistics for Epidemiology}, 1st Edition,
  2004, Chapman & Hall, pp. 73-81
} 

\author{Andri Signorell <andri@signorell.net>, strongly based on code from Tomas Aragon, <aragon@berkeley.edu>}

\seealso{
  \code{\link{RelRisk}} 
}

\examples{
# Case-control study assessing whether exposure to tap water
#   is associated with cryptosporidiosis among AIDS patients

tab <- matrix(c(2, 29, 35, 64, 12, 6), 3, 2, byrow=TRUE)
dimnames(tab) <- list("Tap water exposure" = c("Lowest", "Intermediate", "Highest"), 
                      "Outcome" = c("Case", "Control"))
tab <- Rev(tab, direction="column")

OddsRatio(tab[1:2,])
OddsRatio(tab[c(1,3),])

OddsRatio(tab[1:2,], method="mle")
OddsRatio(tab[1:2,], method="midp")
OddsRatio(tab[1:2,], method="wald", conf.level=0.95)

# in case of zeros consider using glm for calculating OR
dp <- data.frame (a=c(20,7,0,0), b=c(0,0,0,12), t=c(1,0,1,0))
fit <- glm(cbind(a,b) ~ t, binomial, dp)

exp(coef(fit))
}

\keyword{models}
\keyword{multivar}