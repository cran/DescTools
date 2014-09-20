\name{\%overlaps\%}
\alias{\%overlaps\%}
\alias{Interval}

\title{Determines If And How Extensively Two Date Ranges Overlap
%%  ~~function to do ... ~~
}
\description{\%overlaps\% determines if two date ranges overlap at all and returns a logical value. 
Interval returns the number of days of the overlapping part of the 
two date periods. Inspired by the eponymous SQL-functions.
}
\usage{
xp \%overlaps\% yp

Interval(xp, yp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xp}{range 1, consisting of 2 numeric values, typically of type Date.
}
  \item{yp}{range 2, consisting of 2 numeric values, typically of type Date.
}
}
\value{ returns a logical vector (match or not for each element of x). \cr
Interval returns an integer value.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{similar operators: \code{\link{Between}}, \code{\link{\%like\%}} \cr
for calculating the overlapping time: \code{\link{difftime}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
c(Date(2012,1,3), Date(2012,2,3)) \%overlaps\% c(Date(2012,3,1), Date(2012,3,3))
c(Date(2012,1,3), Date(2012,2,3)) \%overlaps\% c(Date(2012,1,15), Date(2012,1,21))

Interval(c(Date(2012,1,3), Date(2012,2,3)), c(Date(2012,3,1), Date(2012,3,3)))


# both ranges are recyled if necessary
Date(2012,1,3) \%overlaps\% c(Date(2012,3,1), Date(2012,3,3))

# works with numerics as well
c(1, 18) \%overlaps\% c(10, 45)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
