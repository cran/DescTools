\name{WrdParagraphFormat}
\alias{WrdParagraphFormat}
\alias{WrdParagraphFormat<-}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get or Set the Paragraph Format in Word
%%  ~~function to do ... ~~
}
\description{\code{WrdParagraphFormat} can be used to get and set the font in Word for the text to be inserted. \code{WrdParagraphFormat} returns the font at the current cursor position.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WrdParagraphFormat(wrd = getOption("lastWord"))
WrdParagraphFormat(wrd) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{value}{the font to be used to the output. This should be defined as a list containing fontname, fontsize, bold and italic flags:\cr \code{list(name="Arial", size=10, bold=FALSE, italic=TRUE)}.
%%     ~~Describe \code{font} here~~
}
  \item{wrd}{the pointer to a word instance. Can be a new one, created by \code{GetNewWrd()}
  or an existing one, created by \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{getOption("lastWord")}.}

}
\value{
a list of the attributes of the font in the current cursor position:
  \item{name}{the fontname}
  \item{size}{the fontsize}
  \item{bold}{bold}
  \item{italic}{italic}

}
\author{
Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{WrdText}}, \code{\link{WrdPlot}}, \code{\link{GetNewWrd}}, \code{\link{GetCurrWrd}}
}
\examples{
\dontrun{ # Windows-specific example
# start word
wrd <- GetNewWrd()

for( i in seq(10, 24, 2)) {
  WrdText(gettextf("This is Arial size \%s \n", i), appendCR=FALSE,
    fontname="Arial", fontsize=i)
}
for( i in seq(10, 24, 2)) {
  WrdText(gettextf("This is Times size \%s \n", i), appendCR=FALSE,
    fontname="Times", fontsize=i)
}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }
