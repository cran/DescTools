\name{Keywords}
\alias{Keywords}
\title{List valid Keywords for R man pages}
\description{
  List valid keywords for R man pages
}
\usage{
Keywords(topic)
}
\arguments{
  \item{topic}{object or man page topic}
}
\details{
  If \code{topic} is provided, return a list of the Keywords associated
  with \code{topic}.  Otherwise, display the list of valid R Keywords
  from the R doc/Keywords file.
}
\author{Gregory R. Warnes \email{greg@warnes.net}}
\seealso{ \code{\link[utils]{help}} }
\examples{
## Show all valid R Keywords
Keywords()

## Show Keywords associated with the 'merge' function
Keywords(merge)
Keywords("merge")
}
\keyword{documentation}
