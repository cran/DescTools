\name{WrdCaption}
\alias{WrdCaption}

\title{Insert Caption to Word}
\description{Insert a caption in a given level to a Word document. The caption is inserted at the current cursor position.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WrdCaption(x, stylename = wdConst$wdStyleHeading1, wrd = getOption("lastWord"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the text of the caption.
%%     ~~Describe \code{x} here~~
}
  \item{stylename}{the name of the heading style in local language or the appropiate word constant.
%%     ~~Describe \code{lvl} here~~
}
  \item{wrd}{the pointer to a word instance. Can be a new one, created by \code{GetNewWrd()} 
  or an existing one, created by \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{getOption("lastWord")}.}
}
\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{
\code{\link{WrdText}}, \code{\link{WrdPlot}}, \code{\link{GetNewWrd}}, \code{\link{GetCurrWrd}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{ # Windows-specific example
wrd <- GetNewWrd()
# insert a title in level 1
WrdCaption("My First Caption level 1", stylename=wdConst$wdStyleHeading1, wrd=wrd)

# works as well for several levels
sapply( 1:5, function(i) 
  WrdCaption(gettextf("My First Caption level %s",i), 
             stylename=eval(parse(text=gettextf("wdConst$wdStyleHeading%s", i))), 
             wrd=wrd)
)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }
