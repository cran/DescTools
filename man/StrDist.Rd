\name{StrDist}
\alias{StrDist}
\title{Compute Distances Between Strings}
\description{
 \code{StrDist} computes distances between strings following to Levenshtein or Hamming method.
}
\usage{
StrDist(x, y, method = "levenshtein", mismatch = 1, gap = 1)
}
\arguments{
  \item{x}{character vector, first string.}
  \item{y}{character vector, second string.}
  \item{method}{character, name of the distance method. This must be 
        \code{"levenshtein"} or \code{"hamming"}. Default is the classical
        Levenshtein distance.}
  \item{mismatch}{numeric, distance value for a mismatch between symbols.}
  \item{gap}{numeric, distance value for inserting a gap.}
}
\details{
  The function computes the Hamming and the Levenshtein (edit) distance of two given strings
  (sequences). The Hamming distance between two vectors is the number mismatches between corresponding entries.
  
  In case of the Hamming distance the two strings must have the same length.
  
  In case of the Levenshtein (edit) distance a scoring and a trace-back matrix are computed
  and are saved as attributes \code{"ScoringMatrix"} and \code{"TraceBackMatrix"}. 
  The numbers in the trace-back matrix reflect insertion of a gap in string \code{y} (1), 
  match/missmatch (2), and insertion of a gap in string \code{x} (3).
}
\value{
  \code{StrDist} returns an object of class \code{"dist"}; cf. \code{\link[stats]{dist}}.
}
\references{
  R. Merkl and S. Waack (2009) \emph{Bioinformatik Interaktiv}. Wiley.
}
\author{Matthias Kohl <Matthias.Kohl@stamats.de> }
\note{
  For distances between strings and for string alignments see also Bioconductor package
  \pkg{Biostrings}
}

\seealso{\code{\link[utils]{adist}}, \code{\link[stats]{dist}} }
\examples{
x <- "GACGGATTATG"
y <- "GATCGGAATAG"
## Levenshtein distance
d <- StrDist(x, y)
d
attr(d, "ScoringMatrix")
attr(d, "TraceBackMatrix")

## Hamming distance
StrDist(x, y, method="hamming")
}
\keyword{math}
