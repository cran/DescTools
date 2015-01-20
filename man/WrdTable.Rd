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

WrdTable(tab, main = NULL, wrd = getOption("lastWord"), row.names = FALSE, \dots)

\method{WrdTable}{Freq}(tab, main = NULL, wrd = getOption("lastWord"), row.names = FALSE, \dots)

\method{WrdTable}{default}(tab, main = NULL, wrd = getOption("lastWord"), row.names = FALSE, 
         fmt = NULL, fontsize = NULL,\dots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab}{the table to be transferred to Word.
%%     ~~Describe \code{tab} here~~
}
  \item{main}{a caption for the plot. This will be inserted by InserCaption in Word. Default is NULL, which will insert nothing.}

  \item{wrd}{the pointer to a word instance. Can be a new one, created by \code{GetNewWrd()} 
  or an existing one, created by \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{getOption("lastWord")}.}

  \item{row.names}{logical, defining whether rownames should be included in the output or not. Default is FALSE.
%%     ~~Describe \code{row.names} here~~
}
\item{fmt}{format string for the table giving the alignment in the columns. l means left, r right and c center alignement. The format code will be recyled.} 
\item{fontsize}{the fontsize of the table}
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
