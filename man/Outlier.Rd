\name{Outlier}
\alias{Outlier}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Outlier 
%%  ~~function to do ... ~~
}
\description{Return outliers following the Tukey's boxplot definition.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Outlier(x, na.rm = FALSE, method = c("boxplot"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.
}
  \item{na.rm}{logical. Should missing values be removed? Defaults to FALSE.
}
  \item{method}{the method to be used. So far only Tukey's boxplot rule is implemented.
%%     ~~Describe \code{method} here~~
}
}
\details{Outlier detection is a tricky problem and should be handled with care. We implement only Tukey's boxplot rule as a rough idea of spotting extreme values.

%%  ~~ If necessary, more details than the description above ~~
}
\value{the values of x lying outside the whiskers in a boxplot
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{boxplot}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

Outlier(d.pizza$temperature, na.rm=TRUE)

# find the corresponding rows
d.pizza[which(d.pizza$temperature \%in\% Outlier(d.pizza$temperature, na.rm=TRUE)),]


# outliers for the drivers
tapply(d.pizza$temperature, d.pizza$driver, Outlier, na.rm=TRUE)

# see also
boxplot(temperature ~ driver, d.pizza)$out

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }
