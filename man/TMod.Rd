\name{TMod}
\alias{TMod}
\alias{ModSummary}
\alias{ModSummary.lm}
\alias{ModSummary.glm}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Comparison Table For Linear Models
%%  ~~function to do ... ~~
}
\description{Collect the coefficients and some qualifying statistics of given linear models and organize it in a table for comparison and reporting.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
TMod(..., FUN = NULL)

ModSummary(x, ...)

\method{ModSummary}{lm}(x, conf.level=0.95, ...)
\method{ModSummary}{glm}(x, conf.level=0.95, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{a (general) linear model object.}
\item{\dots}{a list of (general) linear models.}
\item{conf.level}{the level for the confidence intervals.}
\item{FUN}{function with arguments \code{est}, \code{se}, \code{tval}, \code{pval}, \code{lci}, \code{uci} to display the coefficients.}
}
\details{TMod constructs a comparing table of estimates for regression models.
%%  ~~ If necessary, more details than the description above ~~
}
\value{character table
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{help}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
r.full <- lm(Fertility ~ . , swiss)
r.nox <- lm(Fertility ~ . -Examination - Catholic, swiss)
r.grp <- lm(Fertility ~ . -Education - Catholic + CutQ(Catholic), swiss)
r.gam <- glm(Fertility ~ . , swiss, family=Gamma(link="identity"))
r.gama <- glm(Fertility ~ .- Agriculture , swiss, family=Gamma(link="identity"))

TMod(r.full, r.nox, r.grp, r.gam, r.gama)

# display confidence intervals
TMod(r.full, r.nox, r.gam, FUN = function(est, se, tval, pval, lci, uci){
  gettextf("\%s [\%s, \%s]",
           Format(est, fmt=Fmt("num")),
           Format(lci, digits=3),
           Format(uci, digits=2)
           )
})


# cbind interface is not supported!!
# d.titanic <- reshape(as.data.frame(Titanic),
#                       idvar = c("Class","Sex","Age"),
#                       timevar="Survived",
#                       direction = "wide")
#
# r.glm0 <- glm(cbind(Freq.Yes, Freq.No) ~ 1, data=d.titanic, family="binomial")
# r.glm1 <- glm(cbind(Freq.Yes, Freq.No) ~ Class, data=d.titanic, family="binomial")
# r.glm2 <- glm(cbind(Freq.Yes, Freq.No) ~ ., data=d.titanic, family="binomial")

d.titanic <- Untable(Titanic)

r.glm0 <- glm(Survived ~ 1, data=d.titanic, family="binomial")
r.glm1 <- glm(Survived ~ Class, data=d.titanic, family="binomial")
r.glm2 <- glm(Survived ~ ., data=d.titanic, family="binomial")

TMod(r.glm0, r.glm1, r.glm2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ mod }% use one of  RShowDoc("KEYWORDS")
