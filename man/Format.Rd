\name{Format}
\alias{Format}
\alias{Format.default}
\alias{Format.matrix}
\alias{Format.table}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Format Numbers and Dates
%%  ~~function to do ... ~~
}
\description{Formatting numbers in R often degenerates into a major intellectual challenge for us little guys. We have several functions available and quite often it's hard to work out which one to use, when a special option is needed.
This function wraps those functions and tries to offer a simpler, but still flexible interface.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Format(x, digits = NULL, sci = getOption("scipen"), big.mark="",
       leading = NULL, zero.form = NULL, na.form = NULL,
       fmt = NULL, align = "left", width = NULL, ...)

\method{Format}{table}(x, digits = NULL, sci = getOption("scipen"), big.mark="",
       leading = NULL, zero.form = NULL, na.form = NULL,
       fmt = NULL, align = "left", width = NULL, ...)

\method{Format}{matrix}(x, digits = NULL, sci = getOption("scipen"), big.mark="",
       leading = NULL, zero.form = NULL, na.form = NULL,
       fmt = NULL, align = "left", width = NULL, ...)

\method{Format}{default}(x, digits = NULL, sci = getOption("scipen"), big.mark="",
       leading = NULL, zero.form = NULL, na.form = NULL,
       fmt = NULL, align = NULL, width = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an atomic numerical, typically a vector of real numbers or a matrix of numerical values.
%%     ~~Describe \code{x} here~~
}
  \item{digits}{integer, the desired number of digits after the decimal point. Unlike formatC you will always get this number of digits even if the last digit is 0.
%%     ~~Describe \code{digits} here~~
}
  \item{sci}{integer. The power of 10 to be set when deciding to print numeric values in fixed or exponential notation. Fixed notation will be preferred unless the number is larger than 10^scipen. If just one value is set it will be used for the left border 10^(-scipen) as well as for the right one (10^scipen). A negative and a positive value can also be set independently.
%%     ~~Describe \code{sci} here~~
}
  \item{big.mark}{character; if not empty used as mark between every big.interval decimals before (hence big) the decimal point.
%%     ~~Describe \code{big.mark} here~~
}
  \item{leading}{character string that can be used for setting leading zeros. "000" would make sure that at least 3 digits on the left side will be printed. Setting leading to "drop" will yield results like ".452" for 0.452. The default \code{NULL} will leave the numbers as they are.
%%     ~~Describe \code{drop0leading} here~~
}
  \item{zero.form}{character, string specifying how zeros should be formatted specially. Useful for pretty printing 'sparse' objects.
  If set to NULL (default) no special action will be taken.
%%     ~~Describe \code{pad} here~~
}
  \item{na.form}{character, string specifying how NAs should be formatted specially.
  If set to NULL (default) no special action will be taken.
%%     ~~Describe \code{pad} here~~
}
  \item{fmt}{a format string, allowing to flexibly define special formats. See Details.
%%     ~~Describe \code{fmt} here~~
}
  \item{align}{one out of \code{"left"}, \code{"right"}, \code{"center"}, \code{"dec"}. The values can be aligned left, right, center or at the decimal point. The default is \code{NULL} which would just leave the strings as they are.
%%     ~~Describe \code{align} here~~
}
  \item{width}{integer, the defined width of the strings.
%%     ~~Describe \code{width} here~~
}
\item{\dots}{further arguments to be passed to or from methods. }
}
\details{The argument \code{fmt} can be used for defining several formats. \cr



\tabular{ll}{
dates    \tab   Dates can be formatted with the format codes \code{d}, \code{m} and \code{y} for day, month or year. \cr
\tab Repeting the specific code defines the degree of abbreviation: \cr
\tab  \code{d .....}  day of the month without leading zero (1 - 31) \cr
\tab  \code{dd ....}  day of the month with leading zero (01 - 31)\cr
\tab  \code{ddd ...} abbreviated name for the day of the week (e.g. Mon) in the current user's language  \cr
 \tab                  \code{dddd ..}    full name for the day of the week (e.g. Monday) in the current user's language  \cr
 \tab                  \code{m .....}  month without leading zero (1 - 12) \cr
 \tab                  \code{mm ....}   month with leading zero (01 - 12) \cr
 \tab                 \code{mmm ...}    abbreviated month name (e.g. Jan) in the current user's language \cr
 \tab                \code{mmmm...}     full month name (e.g. January) in the current user's language \cr
  \tab              \code{y .....}   year without century, without leading zero (0 - 99) \cr
 \tab                 \code{yy ....}      year without century, with leading zero (00 - 99) \cr
  \tab              \code{yyyy ..}    year with century. For example: 2005 \cr
\tab\cr
}
\tabular{ll}{
scientific \tab \code{fmt = "e"} forces scientific representation of x. The number of digits, alignment \cr \tab and zero values will be respected.\cr
\tab\cr

percent \tab Setting \code{fmt = "\%"} will divide the given number by 100 and append the \%-sign (without separator).\cr
\tab\cr
p-value \tab \code{fmt = "p"} will wrap the function \code{\link{format.pval}}.\cr
\tab\cr

significance \tab The significance representation of a p-value consisting of *  and . will be produced by setting \code{fmt = "*"}.  \cr
\tab The breaks are set according to the used defaults e.g. in \code{lm} as  \cr
\tab [0, 0.001] = \code{***}  \cr
\tab (0.001, 0.01] = \code{**}  \cr
\tab (0.01, 0.05] = \code{*}  \cr
\tab (0.05, 0.1] = \code{.}  \cr
\tab (0.1,1] = \code{ }\cr
}

\code{fmt} can as well be an object of class \code{fmt} consisting of a list of all arguments of the function.
This allows the format to be stored in a variable or as an option and be used as format template subsequently. If we would want to define an own format for counts, we would write:\cr\cr
\code{fmt.count <- structure(list(digits=0, big.mark="'"), class="fmt")} \cr
\code{Format(4231.2, fmt=fmt.count)} \cr \cr
which again would return the same as \code{Format(4231.2, digits=0, big.mark="'")}.

}
\value{the formatted values as characters. \cr
If x was a matrix, then a the result will also be a matrix. (Hope this will not surprise you...)

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

\seealso{\code{\link{format}}, \code{\link{formatC}}, \code{\link{prettyNum}}, \code{\link{sprintf}}, \code{\link{symnum}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Format(as.Date(c("2014-11-28", "2014-1-2")), fmt="ddd, d mmmm yyyy")

x <- pi * 10^(-10:10)

Format(x, digits=3, fmt="\%", sci=NA)
Format(x, digits=4, sci=4, leading = "drop", width=9, align="dec")


# format a matrix
m <- matrix(runif(100), nrow=10,
            dimnames=list(LETTERS[1:10], LETTERS[1:10]))

Format(m, digits=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }
