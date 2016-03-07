\name{IsDichotomous}
\alias{IsDichotomous}
\alias{Flags}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Test If a Variable Contains Only Two Unique Values
%%  ~~function to do ... ~~
}
\description{Test if a variable contains only two values, and maybe NAs. The variable does not need to be a numerical value, factors and logicals are supported as well.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
IsDichotomous(x)

Flags(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric or integer vector, a logical vector or a factor (ordered and unordered)
%%     ~~Describe \code{x} here~~
}
}
\details{IsDichotomous tests a single variable. Flags returns the names of all the dichotomous variables in a list or data.frame.}
\value{TRUE if x contains only two unique values, FALSE else
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
IsDichotomous(sample(10, 5, replace=TRUE))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arith }
