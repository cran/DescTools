\name{Cstat}
\alias{Cstat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{C Statistic (Area Under the ROC Curve)
%%  ~~function to do ... ~~
}
\description{Calculate the C statitistic for a logistic regression model.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Cstat(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the linear model
%%     ~~Describe \code{x} here~~
}
}
\details{Details
%%  ~~ If necessary, more details than the description above ~~
}
\value{numeric value
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{BrierScore}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
r.glm <- glm(Survived ~ ., data=Untable(Titanic), family=binomial)

Cstat(r.glm)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ mod}% use one of  RShowDoc("KEYWORDS")
