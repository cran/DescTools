\name{ToWrd}
\alias{ToWrd}
\alias{ToWrd.table}
\alias{ToWrd.ftable}
\alias{ToWrd.character}
\alias{ToWrd.lm}
\alias{ToWrd.TOne}
\alias{ToWrd.Freq}
\alias{ToWrd.default}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Send Objects to Word
%%  ~~function to do ... ~~
}
\description{Send objects like tables, ftables, lm tables, TOnes or just simple texts to a MS-Word document.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ToWrd(x, font = NULL, ..., wrd = getOption("lastWord"))

\method{ToWrd}{Freq}(x, font = NULL, main = NULL, ..., wrd = getOption("lastWord"))
\method{ToWrd}{table}(x, font = NULL, main = NULL, align = NULL, autofit = TRUE,
      row.names = FALSE, ..., wrd = getOption("lastWord"))
\method{ToWrd}{ftable}(x, font = NULL, main = NULL, ..., wrd = getOption("lastWord"))
\method{ToWrd}{TOne}(x, font = NULL, main = NULL, align = NULL, autofit = TRUE, ...,
      wrd = getOption("lastWord"))
\method{ToWrd}{lm}(x, font = NULL, ..., wrd = getOption("lastWord"))
\method{ToWrd}{character}(x, font = NULL, para = NULL, style = NULL, ...,
      wrd = getOption("lastWord"))
\method{ToWrd}{default}(x, font = NULL, ..., wrd = getOption("lastWord"))

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the object to be transferred to Word.
%%     ~~Describe \code{x} here~~
}
  \item{font}{the font to be used to the output. This should be defined as a list containing fontname, fontsize, bold and italic flags:\cr \code{list(name="Arial", size=10, bold=FALSE, italic=TRUE)}.
%%     ~~Describe \code{font} here~~
}
\item{para}{list containing paragraph format properties to be applied to the inserted text. For right align the paragraph one can set: \cr
\code{list(alignment="r", LineBefore=0.5)}. See details for the full set of properties.}

  \item{main}{a caption for a table. This will be inserted by InsertCaption in Word and can be listed afterwards in a specific index. Default is NULL, which will insert nothing. Ignored if x is not a table.
%%     ~~Describe \code{main} here~~
}
  \item{align}{character vector giving the alignment of the table columns. \code{"l"} means left, \code{"r"} right and \code{"c"} center alignement. The code will be recyled to the length of thenumber of columns.
%%     ~~Describe \code{align} here~~
}
  \item{autofit}{logical, defining if the columns of table should be fitted to the length of their content.
%%     ~~Describe \code{autofit} here~~
}
  \item{row.names}{logical, defining whether the row.names should be included in the output. Default is \code{FALSE}.
%%     ~~Describe \code{row.names} here~~
}
\item{style}{character, name of a style to be applied to the inserted text.}
  \item{\dots}{further arguments to be passed to or from methods.
%%     ~~Describe \code{\dots} here~~
}
  \item{wrd}{the pointer to a word instance. Can be a new one, created by \code{GetNewWrd()}
  or an existing one, created by \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{getOption("lastWord")}.
%%     ~~Describe \code{wrd} here~~
}
}
\value{if x is a table a pointer to the table will be returned
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\details{The paragraph format can be defined by means of these properties:
LeftIndent, RightIndent, SpaceBefore, SpaceBeforeAuto, SpaceAfter, SpaceAfterAuto, LineSpacingRule,
Alignment, WidowControl, KeepWithNext, KeepTogether, PageBreakBefore, NoLineNumber, Hyphenation,
FirstLineIndent, OutlineLevel, CharacterUnitLeftIndent, CharacterUnitRightIndent, CharacterUnitFirstLineIndent,
LineUnitBefore, LineUnitAfter, MirrorIndents.
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{GetNewWrd}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
t1 <- TOne(x = d.pizza[, c("temperature","delivery_min","driver","wine_ordered")],
           grp=d.pizza$wine_delivered)

wrd <- GetNewWrd()
ToWrd(t1, font=list(name="Algerian"), wrd=wrd)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }% use one of  RShowDoc("KEYWORDS")
