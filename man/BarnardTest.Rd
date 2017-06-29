\name{BarnardTest}
\alias{BarnardTest}
\title{ Barnard's Unconditional Test}
\description{
Barnard's unconditional test for superiority applied to \eqn{2 \times 2}{2x2} contingency tables
using Score or Wald statistics for the difference between two binomial proportions.
}
\usage{
BarnardTest(x, y = NULL, alternative = c("two.sided", "less", "greater"),
            dp = 0.001, pooled = TRUE)
}
\arguments{

 \item{x}{a numeric vector or a two-dimensional contingency table in matrix form. \code{x} and \code{y} can also both be factors. }
 \item{y}{a factor object; ignored if \code{x} is a matrix.}

  \item{dp}{ The resolution of the search space for the nuisance parameter }
  \item{pooled}{ Z statistic with pooled (Score) or unpooled (Wald) variance }

  \item{alternative}{a character string specifying the alternative
    hypothesis, must be one of \code{"two.sided"} (default),
    \code{"greater"} or \code{"less"}.  You can specify just the initial
    letter.}

}


\value{
   A list with class \code{"htest"} containing the following components:
   \item{p.value}{the p-value of the test.}
   \item{estimate}{an estimate of the nuisance parameter where the p-value is maximized.}
   \item{alternative}{a character string describing the alternative
     hypothesis.}
   \item{method}{the character string
     \code{"Barnards Unconditional 2x2-test"}.}
   \item{data.name}{a character string giving the names of the data.}
  \item{statistic.table }{The contingency tables considered in the analysis represented by 'n1' and 'n2', their scores, and whether they are included in the one-sided (1), two-sided (2) tests, or not included at all (0)}
   \item{nuisance.matrix }{Nuisance parameters, \eqn{p}, and the corresponding p-values for both one- and two-sided tests}

 }

\details{
If \code{x} is a matrix, it is taken as a two-dimensional contingency
   table, and hence its entries should be nonnegative integers.
   Otherwise, both \code{x} and \code{y} must be vectors of the same
   length.  Incomplete cases are removed, the vectors are coerced into
   factor objects, and the contingency table is computed from these.

For a 2x2 contingency table, such as \eqn{X=[n_1,n_2;n_3,n_4]}, the normalized difference in proportions between the two categories, given in each column, can be written with pooled variance (Score statistic) as
\deqn{T(X)=\frac{\hat{p}_2-\hat{p}_1}{\sqrt{\hat{p}(1-\hat{p})(\frac{1}{c_1}+\frac{1}{c_2})}},}
where \eqn{\hat{p}=(n_1+n_3)/(n_1+n_2+n_3+n_4)}, \eqn{\hat{p}_2=n_2/(n_2+n_4)}, \eqn{\hat{p}_1=n_1/(n_1+n_3)}, \eqn{c_1=n_1+n_3} and \eqn{c_2=n_2+n_4}. Alternatively, with unpooled variance (Wald statistic), the difference in proportions can we written as
\deqn{T(X)=\frac{\hat{p}_2-\hat{p}_1}{\sqrt{\frac{\hat{p}_1(1-\hat{p}_1)}{c_1}+\frac{\hat{p}_2(1-\hat{p}_2)}{c_2}}}.}
The probability of observing \eqn{X} is
\deqn{P(X)=\frac{c_1!c_2!}{n_1!n_2!n_3!n_4!}p^{n_1+n_2}(1-p)^{n_3+n_4},}
where \eqn{p} is the unknown nuisance parameter.

Barnard's test considers all tables with category sizes \eqn{c_1} and \eqn{c_2} for a given \eqn{p}. The p-value is the sum of probabilities of the tables having a score in the rejection region, e.g. having significantly large difference in proportions for a two-sided test. The p-value of the test is the maximum p-value calculated over all \eqn{p} between 0 and 1.
}
\references{
Barnard, G.A. (1945) A new test for 2x2 tables. \emph{Nature}, 156:177.

Barnard, G.A. (1947) Significance tests for 2x2 tables. \emph{Biometrika}, 34:123-138.

Suissa, S. and Shuster, J. J. (1985), Exact Unconditional Sample Sizes for the 2x2 Binomial Trial, \emph{Journal of the Royal Statistical Society}, Ser. A, 148, 317-327.

Cardillo G. (2009) MyBarnard: a very compact routine for Barnard's exact test on 2x2 matrix. \url{http://ch.mathworks.com/matlabcentral/fileexchange/25760-mybarnard}

Galili T. (2010) \url{http://www.r-statistics.com/2010/02/barnards-exact-test-a-powerful-alternative-for-fishers-exact-test-implemented-in-r/}

Lin C.Y., Yang M.C. (2009) Improved p-value tests for comparing two independent binomial proportions. \emph{Communications in Statistics-Simulation and Computation}, 38(1):78-91.

Trujillo-Ortiz, A., R. Hernandez-Walls, A. Castro-Perez, L. Rodriguez-Cardozo N.A. Ramos-Delgado and R. Garcia-Sanchez. (2004). Barnardextest:Barnard's Exact Probability Test. A MATLAB file. [WWW document]. \url{http://www.mathworks.com/}
}
\author{
Kamil Erguler, <k.erguler@cyi.ac.cy>, Peter Calhoun <calhoun.peter@gmail.com>, Rodrigo Duprat, Andri Signorell <andri@signorell.net> (interface)
}
\seealso{\code{\link{fisher.test}}}

\examples{
tab <- as.table(matrix(c(8, 14, 1, 3), nrow=2,
                dimnames=list(treat=c("I","II"), out=c("I","II"))))
BarnardTest(tab)

# Plotting the search for the nuisance parameter for a one-sided test
bt <- BarnardTest(tab)
plot(bt$nuisance.matrix[, 1:2],
     t="l", xlab="nuisance parameter", ylab="p-value")

# Plotting the tables included in the p-value
ttab <- as.table(matrix(c(40, 14, 10, 30), nrow=2,
                 dimnames=list(treat=c("I","II"), out=c("I","II"))))

bt <- BarnardTest(ttab)
bts <- bt$statistic.table
plot(bts[, 1], bts[, 2],
     col=hsv(bts[, 4] / 4, 1, 1),
     t="p", xlab="n1", ylab="n2")

# Plotting the difference between pooled and unpooled tests
bts <- BarnardTest(ttab, pooled=TRUE)$statistic.table
btw <- BarnardTest(ttab, pooled=FALSE)$statistic.table
plot(bts[, 1], bts[, 2],
     col=c("black", "white")[1 + as.numeric(bts[, 4]==btw[, 4])],
     t="p", xlab="n1", ylab="n2")
}

\keyword{nonparametric}
\keyword{htest}
