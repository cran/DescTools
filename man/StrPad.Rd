\name{StrPad}
\alias{StrPad}
\title{Pad a String With Justification}
\description{
  \code{StrPad} will fill a string x with defined characters to fit a given length.
}
\usage{
StrPad(x, len, pad = " ", adj = c("left", "right", "center"))}
\arguments{
  \item{x}{string to be padded.}
  \item{len}{resulting length of padded string.}
  \item{pad}{string to pad with. Will be repeated as often as necessary. Default is " ".}
  \item{adj}{adjustement of the old string, one of \code{"left"}, \code{"right"}, \code{"center"}. If set to \code{"left"} the old string will be adjusted on the left and the new characters will be filled in on the right side.}
}
\value{
the string 
}
\details{
If a string x has more characters than len, it will be chopped on the length of len.
}

\examples{
StrPad("My string", 25, "XoX", "center")
 # [1] "XoXXoXXoMy stringXXoXXoXX"
}
\author{
Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>\cr
some extensions Andri Signorell <andri@signorell.net>
}
\keyword{character}
