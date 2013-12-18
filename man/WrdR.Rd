\name{WrdR}
\alias{WrdR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Insert a R Command and It's Output in a Word Document
%%  ~~function to do ... ~~
}
\description{Insert an R Command and It's Output in Word document. Helpful for documenting tasks.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WrdR(x, wrd = getOption("lastWord"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{R command as text to be evaluated.
%%     ~~Describe \code{x} here~~
}
  \item{wrd}{the pointer to a word instance. Can be a new one, created by \code{GetNewWrd()} 
  or an existing one, created by \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{getOption("lastWord")}.}
}
\details{The command text will be placed in a Word document and formatted in italics. 
The result will be written in bold fontface.
%%  ~~ If necessary, more details than the description above ~~
}

\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}


\seealso{\code{\link{WrdText}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{# Windows-specific example
\dontrun{
wrd <- GetNewWrd()
WrdR("sapply(iris[,-5], mean)", wrd=wrd)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip}

