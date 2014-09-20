\name{WrdText}
\alias{WrdText}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Insert Normal Text to Word
%%  ~~function to do ... ~~
}
\description{Write text in defined font and append a carriage return if requested.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WrdText(txt, fontname = "Consolas", fontsize = 7, bold = FALSE, italic = FALSE, 
        appendCR = TRUE, wrd = getOption("lastWord"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{txt}{the text to be inserted.
%%     ~~Describe \code{txt} here~~
}
  \item{fontname}{the font of the text.
%%     ~~Describe \code{fontname} here~~
}
  \item{fontsize}{the fontsize of the text.
%%     ~~Describe \code{fontsize} here~~
}
  \item{bold}{should the text be bold?
%%     ~~Describe \code{bold} here~~
}
  \item{italic}{should the text be italic?
%%     ~~Describe \code{italic} here~~
}
  \item{appendCR}{should a carriage return be appended to the text. Default is TRUE.
%%     ~~Describe \code{appendCR} here~~
}
  \item{wrd}{the pointer to a word instance. Can be a new one, created by \code{GetNewWrd()} 
  or an existing one, created by \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{getOption("lastWord")}.}
}
\value{
Returns a list of the attributes of the font in the current cursor position.
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{name   }{the fontname}
  \item{size   }{the fontsize}
  \item{bold   }{bold}
  \item{italic }{italic}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{
\code{\link{WrdSetFont}}, \code{\link{WrdPlot}}, \code{\link{GetNewWrd}}, \code{\link{GetCurrWrd}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{ # Windows-specific example
# Let's write a story
data(d.diamonds)

# start word
wrd <- GetNewWrd()

WrdCaption("My Word-Story", stylename=wdConst$wdStyleHeading1)

WrdText("This will be the structure of d.diamonds:\n\n", appendCR=FALSE, 
        fontname="Arial", fontsize=10)
WrdText(capture.output(str(d.diamonds)))

wrd[["Selection"]]$InsertBreak(wdConst$wdPageBreak)
WrdText("Lets insert a table (and this ist written Times)!", fontname="Times", 
        fontsize=12, appendCR=FALSE, bold=T)

# insert table
wrd[["ActiveDocument"]][["Tables"]]$Add( wrd[["Selection"]][["Range"]], 
                                         NumRows=2, NumColumns=2 )
WrdText("First Cell", fontname="Arial", fontsize=10)
wrd[["Selection"]]$MoveRight( Unit=wdConst$wdCell, Count=1 )
WrdText("Second Cell")
wrd[["Selection"]]$MoveRight( Unit=wdConst$wdCell, Count=1 )

wrd[["Selection"]]$MoveRight( Unit=wdConst$wdCharacter, Count=2, 
                              Extend=wdConst$wdExtend )
wrd[["Selection"]][["Cells"]]$Merge()
WrdText("This cell was merged....", fontname="Arial", fontsize=10)


# exit the table range
wrd[["Selection"]]$EndOf( wdConst$wdTable )

wrd[["Selection"]]$MoveRight( wdConst$wdCharacter, 2, 0 )
wrd[["Selection"]]$TypeParagraph()

# let's insert a bookmark
wrd[["ActiveDocument"]][["Bookmarks"]]$Add("myBookmark")
wrd[["Selection"]]$MoveRight( Unit=wdConst$wdCharacter, Count=1 )
WrdText("\n\n", fontname="Arial", fontsize=10)

# set border for a paragraph
BorderBottom <- wrd[["Selection"]][["ParagraphFormat"]][["Borders"]]$Item(-3)
BorderBottom[["LineStyle"]] <- 1
wrd[["Selection"]]$MoveRight( wdConst$wdCharacter, 1, 0 )
WrdText("This paragraph has a Border", fontname="Arial", fontsize=10)
wrd[["Selection"]]$MoveRight( wdConst$wdCharacter, 2, 0 )
WrdText("\n\n", fontname="Arial", fontsize=10)

# insert new landscape section
wrd[["Selection"]]$InsertBreak(wdConst$wdSectionBreakNextPage)
wrd[["Selection"]][["PageSetup"]][["Orientation"]] <- wdConst$wdOrientLandscape

# new text
WrdText("Text in landscape", fontname="Impact", fontsize=20)
wrd[["Selection"]][["PageSetup"]][["Bottommargin"]] <- 4 * 72
wrd[["Selection"]][["PageSetup"]][["Leftmargin"]] <- 4 * 72
# wrd[["Selection"]][["PageSetup"]][["Topmargin"]] <- 4 * 72
# wrd[["Selection"]][["PageSetup"]][["Rightmargin"]] <- 4 * 72  

# return to the bookmark
wrd[["Selection"]]$GoTo( wdConst$wdGoToBookmark, 0, 0, "myBookmark") 

# and insert text
WrdText("Back again")

# goto end of document
wrd[["Selection"]]$EndKey(wdConst$wdStory)

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }
