\name{WrdSetFont}
\alias{WrdSetFont}
\alias{WrdGetFont}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Set the Font in Word 
%%  ~~function to do ... ~~
}
\description{\code{WrdSetFont} sets the font in Word for the text to be inserted. \code{WrdGetFont} returns the font at the current cursor position.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WrdSetFont(fontname = "Consolas", fontsize = 7, bold = FALSE, italic = FALSE, 
           wrd = getOption("lastWord"))
WrdGetFont(wrd = getOption("lastWord"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fontname}{the name of the font as defined by Windows.
%%     ~~Describe \code{fontname} here~~
}
  \item{fontsize}{the size of the font in points.
%%     ~~Describe \code{fontsize} here~~
}
  \item{bold, italic}{does the expected.
%%     ~~Describe \code{fontsize} here~~
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
