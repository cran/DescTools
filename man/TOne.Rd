\name{TOne}
\alias{TOne}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create Table One Describing Baseline Characteristics
%%  ~~function to do ... ~~
}
\description{
Create a table summarizing continuous, categorical and dichotomous variables, optionally stratified by one or more variables, while performing adequate statistical tests.
}
\usage{
TOne(x, grp = NA, add.length = TRUE, colnames = NULL, vnames = NULL, total = TRUE,
     align = "\\\\l", FUN = NULL, NUMTEST = NULL, numtestlab = NULL, intref = "high")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a data.frame containing all the variables to be included in the table.
%%     ~~Describe \code{x} here~~
}
  \item{grp}{the grouping variable.
%%     ~~Describe \code{grp} here~~
}
  \item{add.length}{logical. If set to \code{TRUE} (default), a row with the lengths of the group will be inserted as first row of the table.
%%     ~~Describe \code{grp} here~~
}
  \item{colnames}{a vector of columnnames for the result table.
%%     ~~Describe \code{grp} here~~
}
  \item{vnames}{a vector of variablenames to be placed in the first column instead of the real names.
%%     ~~Describe \code{grp} here~~
}
\item{total}{logical, \code{TRUE} if totals should be added.}

  \item{align}{the character on whose position the strings will be aligned. Left alignment can be requested by setting \code{sep = "\\\\l"}, right alignment by \code{"\\\\r"} and center alignment by \code{"\\\\c"}. Mind the backslashes, as if they are omitted, strings would be aligned to the \bold{character} l, r or c respectively. Default value is \code{"\\\\l"}, thus left alignment.
%%     ~~Describe \code{sep} here~~
}
\item{FUN}{the function to be used as location and dispersion measure (typically \code{mean}/\code{sd} or alternatively \code{median}/\code{IQR}). See examples.}
\item{NUMTEST}{the function to be used to compare location (default \code{kruskal.test}). Must be defined as function with arguments \code{(x, g)}, generating something similar to a p.value. (See examples.) }
\item{numtestlab}{string, describing the test for numeric variables. Default is "Kruskal-Wallis test". Alternative could be "ANOVA".}
\item{intref}{one out of \code{"high"} (default) or \code{"low"}, defining which value of a dichotomous numeric or logical variable should be reported. Usually this will be \code{1} or \code{TRUE}. Setting it to \code{"low"} will report the lower value \code{0} or \code{FALSE}. }
}
\details{There are three types of variables currently implemented: numeric, factor and variables with only two levels.
%%  ~~ If necessary, more details than the description above ~~
}
\value{a character matrix
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

\seealso{ \code{\link{WrdTable}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
opt <- DescToolsOptions()

# define some special formats for count data, percentages and numeric results
# (those will be supported by TOne)
Fmt(abs=as.fmt(digits=0, big.mark="'"))
Fmt(per=as.fmt(digits=1, fmt="\%"))
Fmt(num=as.fmt(digits=1, big.mark="'"))

TOne(x = d.pizza[, c("temperature","delivery_min","driver","wine_ordered")],
     grp=d.pizza$quality)

# define median/IQR as describing functions for the numeric variables
TOne(iris[, -5], iris[, 5],
  FUN = function(x) gettextf("\%s / \%s",
         Format(median(x, na.rm=TRUE), digits=1),
         Format(IQR(x, na.rm=TRUE), digits=3))
)

# replace kruskal.test by ANOVA and report the p.value
TOne(x = iris[, -5], grp = iris[, 5],
     FUN = function(x) gettextf("\%s / \%s",
            Format(mean(x, na.rm=TRUE), digits=1),
            Format(sd(x, na.rm=TRUE), digits=3)),
     NUMTEST = function(x, g)
            Format(summary(aov(x ~ g))[[1]][1, "Pr(>F)"], fmt="p"),
     numtestlab = "ANOVA"
)

# dichotomous integer or logical values can be reported by the high or low value
x <- sample(x = c(0,1), size = 100, prob = c(0.3, 0.7), replace = TRUE)
y <- sample(x = c(0,1), size = 100, prob = c(0.3, 0.7), replace = TRUE)==1
z <- factor(sample(x = c(0,1), size = 100, prob = c(0.3, 0.7), replace = TRUE))
g <- sample(x = letters[1:4], size = 100, replace = TRUE)
d.set <- data.frame(x=x, y=y, z=z, g=g)

TOne(d.set[1:3], d.set$g, intref = "low")
TOne(d.set[1:3], d.set$g, intref = "high")

# intref would not control factors, use relevel to change reported value
TOne(data.frame(z=relevel(z, "1")), g)
TOne(data.frame(z=z), g)



options(opt)

\dontrun{  --  Send the whole stuff to Word

wrd <- GetNewWrd()
ToWrd(TOne(x = d.pizza[, c("temperature","delivery_min","driver","wine_ordered")],
           grp=d.pizza$quality))
           }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO }
