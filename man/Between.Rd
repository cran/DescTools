\name{Between}
\alias{Between}
\alias{\%()\%}
\alias{\%(]\%}
\alias{\%[)\%}
\alias{\%[]\%}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Between Operators Check, if a Value Lies Within a Given Range
}
\description{
The between operators are used to check, whether given values x lie within a defined range. The values can be numbers, text or dates. 
Ordered factors are supported.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
x \%()\% rng
x \%(]\% rng
x \%[)\% rng
x \%[]\% rng
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  is a variable with at least ordinal scale, usually a numeric value, but can be an ordered factor or a text as well. Texts would be treated alphabetically.
%%     ~~Describe \code{x} here~~
}
  \item{rng}{
  a vector of two values, defining the minimum and maximum of the range for x
%%     ~~Describe \code{rng} here~~
}
}
\details{The between operators basically combine two conditional statements into one and simplify so the query process.\cr
They are merely a wrapper for:   \code{ifelse (x >= rng[1] & x <= rng[2], TRUE, FALSE)}, 
where the round bracket  \code{(}  means "strictly greater than >" and the square bracket  \code{[}  means ">=".  Numerical values of x will be handled by C-code, which is significantly faster than two comparisons in R (especially when x is huge).  
.\cr
SQL-guys might like these kind of thinking.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A logical vector of the same length as x. 
}
\author{
Andri Signorell <andri@signorell.net> based on C-code by Kevin Ushey <kevinushey@gmail.com>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{if}}, \code{\link{ifelse}}, \code{\link{Comparison}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

x <- 1:9
x \%[]\% c(3,5)
c(x,NA) \%[]\% c(3,5)

x \%(]\% c(3,5)

# no result when from > to:
x \%[]\% c(5,3)
x \%(]\% c(5,5)

# no problem:
ordered(x) \%[]\% c(3,5)

# not meaningful:
factor(x) \%[]\% c(3,5)

# characters
letters[letters \%(]\% c("d","h")]

data(d.pizza)
x <- levels(d.pizza$driver)
x \%[]\% c("C","G")

# select diamonds with a price between 2400 and 2510
data(d.diamonds)
d.diamonds[d.diamonds$price \%[]\% c(2400,2510),]

# use it with an ordered factor and select all diamonds with 
#   symmetry between G (included) and X (excluded).
mean(d.diamonds[d.diamonds$symmetry \%[)\% c("G","X"),"price"])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{logic}


