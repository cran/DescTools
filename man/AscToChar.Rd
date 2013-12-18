\name{AscToChar}
\alias{AscToChar}
\alias{CharToAsc}

\title{Converts ASCII Codes to Characters and Vice Versa
}
\description{AscToChar returns a character for each ASCII code (integer) supplied.\cr
CharToAsc returns integer codes in \code{0:255} for each (one byte) character in \code{strings}.
}
\usage{
AscToChar(i)
CharToAsc(x)
}
\arguments{
\item{i}{numeric (integer) vector of values in \code{1:255}.
}
\item{x}{\code{\link{character}} vector.
}}
\details{Only codes in \code{1:127} make up the ASCII encoding which should be
  identical for all \R versions, whereas the \emph{\sQuote{upper}} half
  is often determined from the ISO-8859-1 (aka \dQuote{ISO-Latin 1)}
  encoding, but may well differ, depending on the locale setting, see
  also \code{\link{Sys.setlocale}}.

  Note that \code{0} is no longer allowed since, \R does not allow
  \code{\\0} aka \code{nul} characters in a string anymore.
}
\value{
 \code{AscToChar} and \code{CharToAsc} return a
  vector of the same length as their argument.
}
\author{unknown guy out there, help text partly taken from M. Maechler's \pkg{sfmisc}.
}

\examples{

(x <- CharToAsc("Silvia"))
paste(AscToChar(x), collapse="")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
