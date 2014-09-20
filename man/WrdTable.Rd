\name{WrdTable}
\alias{WrdTable}
\alias{WrdTable.Freq}
\alias{WrdTable.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Produces a Table in Word
%%  ~~function to do ... ~~
}
\description{Creates a table in MS-Word.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{

WrdTable(tab, wrd = getOption("lastWord"), row.names = FALSE, \dots)

\method{WrdTable}{Freq}(tab, wrd = getOption("lastWord"), row.names = FALSE, \dots)
\method{WrdTable}{default}(tab, wrd = getOption("lastWord"), row.names = FALSE, \dots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab}{the table to be transferred to Word.
%%     ~~Describe \code{tab} here~~
}
  \item{wrd}{the pointer to a word instance. Can be a new one, created by \code{GetNewWrd()} 
  or an existing one, created by \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{getOption("lastWord")}.}

  \item{row.names}{
%%     ~~Describe \code{row.names} here~~
}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{A tricky problem, still unsolved... This is experimental code.
%%  ~~ If necessary, more details than the description above ~~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{GetNewWrd}}
}
\keyword{ print }
