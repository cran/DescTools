\name{Conf}
\alias{Conf}
\alias{Conf.table}
\alias{Conf.default}
\alias{Conf.matrix}
\alias{Conf.rpart}
\alias{Conf.multinom}
\alias{Conf.glm}
\alias{Conf.randomForest}
\alias{Conf.svm}
\alias{Conf.regr}
\alias{plot.Conf}
\alias{print.Conf}
\alias{Sens}
\alias{Spec}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Confusion Matrix And Associated Statistics
%%  ~~function to do ... ~~
}
\description{Calculates a cross-tabulation of observed and predicted classes with associated statistics.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Conf(x, ...)

\method{Conf}{table}(x, pos = NULL, ...) 
\method{Conf}{matrix}(x, pos = NULL, ...)
\method{Conf}{default}(x, ref, pos = NULL, na.rm = TRUE, ...) 
  
\method{Conf}{rpart}(x, ...)
\method{Conf}{multinom}(x, ...)
\method{Conf}{glm}(x, cutoff = 0.5, ...)
\method{Conf}{randomForest}(x, ...)
\method{Conf}{svm}(x, ...)
\method{Conf}{regr}(x, ...)

\method{plot}{Conf}(x, main="Confusion Matrix", ...)

\method{print}{Conf}(x, digits = max(3, getOption("digits") - 3), ...) 

Sens(x, ...) 
Spec(x, ...) 

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector, normally a factor, of predicted classes (for the default method) or an object of class \code{\link{table}}.
  }
\item{ref}{a vector, normally a factor, of classes to be used as the reference.}
\item{pos}{a character string that defines the factor level corresponding to the "positive" results.}
\item{cutoff}{the cutoff point for separating the classes. This is only used if x is a glm-model and ignored else. }

  \item{main}{overall title for the plot. }

  \item{digits}{controls the number of digits to print.
}

  \item{na.rm}{a logical value indicating whether or not missing values should be removed. Defaults to FALSE.

%%     ~~Describe \code{na.rm} here~~
}


  \item{\dots}{further arguments to be passed to or from methods.}
  
  }
\details{
The functions requires that the factors have exactly the same levels.

For two class problems, the sensitivity, specificity, positive
predictive value and negative predictive value is calculated using the
\code{positive} argument. Also, the prevalence of the "event" is computed from the 
data (unless passed in as an argument), the detection rate (the rate of true events also 
predicted to be events) and the detection prevalence (the prevalence of predicted events).

Suppose a 2x2 table with notation

 \tabular{rcc}{
                    \tab Reference \tab          \cr
         Predicted  \tab Event     \tab No Event \cr
         Event      \tab A         \tab B        \cr
         No Event   \tab C         \tab D        \cr
       }

The formulas used here are:
\deqn{Sensitivity = A/(A+C)}
\deqn{Specificity = D/(B+D)}
\deqn{Prevalence = (A+C)/(A+B+C+D)}
\deqn{PPV = (sensitivity * Prevalence)/((sensitivity*Prevalence) + ((1-specificity)*(1-Prevalence)))}
\deqn{NPV = (specificity * (1-Prevalence))/(((1-sensitivity)*Prevalence) + ((specificity)*(1-Prevalence)))}
\deqn{Detection Rate =  A/(A+B+C+D)}
\deqn{Detection Prevalence =  (A+B)/(A+B+C+D)}

See the references for discusions of the first five formulas.

For more than two classes, these results are
calculated comparing each factor level to the remaining levels
(i.e. a "one versus all" approach). 

The overall accuracy and unweighted Kappa statistic are calculated. A p-value from McNemar's test is also computed using \code{\link[stats]{mcnemar.test}} (which can produce \code{NA} values with sparse tables).

The overall accuracy rate is computed along with a 95 percent confidence interval for this rate (using \code{\link[DescTools]{BinomCI}}) and a one-sided test to see if the accuracy is better than the "no information rate," which is taken to be the largest class percentage in the data.

 The sensitivity is defined as the proportion of positive results out of the number of
 samples which were actually positive. When there are no positive results, sensitivity is
 not defined and a value of \code{NA} is returned. Similarly, when there are no negative 
 results, specificity is not defined and a value of \code{NA} is returned. Similar 
 statements are true for predictive values.

 
}
\value{
a list with elements
  \item{table}{the results of \code{table} on \code{data} and  \code{reference}}
  \item{positive}{the positive result level}
  \item{overall}{a numeric vector with overall accuracy and Kappa statistic values}
  \item{byClass}{the sensitivity, specificity, positive predictive value, negative predictive value, prevalence, dection rate and detection prevalence for each class. For two class systems, this is calculated once using the \code{positive} argument}

}
\references{
Kuhn, M. (2008) Building predictive models in R using the caret package \emph{Journal of Statistical Software}, (\url{http://www.jstatsoft.org/v28/i05/}).

%% ~put references to the literature/web site here ~
}
\author{
Andri Signorell <andri@signorell.net> \cr
rewritten based on the ideas of \code{\link[caret]{confusionMatrix}} by Max Kuhn <Max.Kuhn@pfizer.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{OddsRatio}}, \code{\link{RelRisk}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# let tab be a confusion table
tab <- structure(c(233L, 29L, 56L, 74L), .Dim = c(2L, 2L), 
          .Dimnames = structure(list(pred = c("0", "1"), obs = c("0", "1")), 
             .Names = c("pred", "obs")))

Conf(tab, pos="1")


pred <- Untable(tab)[,"pred"]
obs <- Untable(tab)[,"obs"]

Conf(x = pred, ref = obs)
Conf(x = pred, ref = obs, pos="1")

Sens(tab)   # Sensitivity
Spec(tab)   # Specificity
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivar}