\name{Stamp}
\alias{Stamp}
\title{Date/Time/Directory Stamp the Current Plot}
\description{
Date-time stamp the current plot in the extreme lower right
corner. Optionally add the current working directory and arbitrary other
text to the stamp.
}
\usage{
Stamp(txt, wdpath = FALSE, time = FALSE, las = 1, cex = 0.6)
}
\arguments{
  \item{txt}{an optional single text string}
  \item{wdpath}{set to \code{TRUE} to add the current working directory
	name to the stamp}
  \item{time}{set to \code{FALSE} to use the date without the time}
  \item{las}{numeric in {1,3}, defining direction of the text. 1 (default) means horizontal, 3 vertical.}
  \item{cex}{numeric \bold{c}haracter \bold{ex}pansion factor; multiplied by \code{par("cex")} yields the final character size. Defaults to 0.6.}
}
\details{
  For \R results may not be satisfactory if \code{par(mfrow=)} is in effect.
}
\seealso{\code{\link{text}}}

\author{Frank E Harrell Jr <f.harrell@vanderbilt.edu>}
\examples{
plot(1:20)
Stamp(wdpath=TRUE)
}
\keyword{aplot}
