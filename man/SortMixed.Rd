% $Id: SortMixed.Rd 1433 2010-05-01 22:03:03Z warnes $
%
\name{SortMixed}
\alias{SortMixed}
\alias{OrderMixed}
\title{Order or Sort Strings With Embedded Numbers So That The Numbers
  Are In The Correct Order}
\description{
  These functions sort or order character strings containing numbers so
  that the numbers are numerically sorted rather than sorted by
  character value.  I.e. "Asprin 50mg" will come before "Asprin 100mg".
  In addition, case of character strings is ignored so that "a", will
  come before "B" and "C".
}
\usage{
SortMixed(x)
}
\arguments{
  \item{x}{ Character vector to be sorted }
}
\details{
  I often have character vectors (e.g. factor labels) that contain
  both text and numeric data, such as compound and dose.  This function
  is useful for sorting these character vectors into a logical order.

  It does so by splitting each character vector into a sequence of
  character and numeric sections, and then sorting along these sections,
  with numbers being sorted by numeric value (e.g. "50" comes before
  "100"), followed by characters strings sorted by character
  value (e.g. "A" comes before "B").

  Empty strings are always sorted to the front of the list, and \code{NA}
  values to the end.
}
\value{
  \code{OrderMixed} returns a vector giving the sort order of the input
  elements. \code{SortMixed} returns the sorted vector.
}
\author{ Gregory R. Warnes <greg@warnes.net> }
\seealso{ \code{\link[base]{sort}}, \code{\link[base]{order}} }
\examples{
# compound & dose labels
Treatment <- c("Control", "Asprin 10mg/day", "Asprin 50mg/day",
               "Asprin 100mg/day", "Acetomycin 100mg/day",
               "Acetomycin 1000mg/day")

# ordinary sort puts the dosages in the wrong order
sort(Treatment)

# but SortMixed does the 'right' thing
SortMixed(Treatment)

# Here is a more complex example
x <- rev(c("AA 0.50 ml", "AA 1.5 ml", "AA 500 ml", "AA 1500 ml",
           "EXP 1", "AA 1e3 ml", "A A A", "1 2 3 A", "NA", NA, "1e2",
           "", "-", "1A", "1 A", "100", "100A", "Inf"))

OrderMixed(x)

SortMixed(x)
# notice that plain numbers, including 'Inf' show up before strings.

}
\keyword{univar}
\keyword{manip}


\concept{natural sort}
\concept{dictionary sort}

