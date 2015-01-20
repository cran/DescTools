\name{Stamp}
\alias{Stamp}
\title{Date/Time/Directory Stamp the Current Plot}
\description{
Date-time stamp the current plot in the extreme lower right
corner. Optionally add the current working directory and arbitrary other
text to the stamp.
}
\usage{
Stamp(txt, pwd = FALSE, time = FALSE)
}
\arguments{
  \item{txt}{an optional single text string}
  \item{pwd}{set to \code{TRUE} to add the current working directory
	name to the stamp}
  \item{time}{set to \code{FALSE} to use the date without the time}
}
\details{
  For \R results may not be satisfactory if \code{par(mfrow=)} is in effect.
}
\author{Frank E Harrell Jr <f.harrell@vanderbilt.edu>}
\examples{
plot(1:20)
Stamp(pwd=TRUE)
}
\keyword{aplot}
