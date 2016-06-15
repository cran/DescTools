\name{WrdTableBorders}
\alias{WrdTableBorders}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draw Borders to a Word Table
%%  ~~function to do ... ~~
}
\description{Drawing borders in a Word table is quite tedious. This function allows to select any range and draw border lines around it.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WrdTableBorders(wtab, from = NULL, to = NULL, border = wdConst$wdBorderBottom,
                lty = wdConst$wdLineStyleSingle, wrd)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wtab}{a pointer to a Word table as returned by \code{\link{WrdTable}} or \code{\link{TOne}}.
%%     ~~Describe \code{wtab} here~~
}
  \item{from}{integer, a vector with two elements specifying the left upper bound of the cellrange.
%%     ~~Describe \code{from} here~~
}
  \item{to}{integer, a vector with two elements specifying the right bottom of the cellrange.
%%     ~~Describe \code{to} here~~
}
  \item{border}{a Word constant (\code{wdConst$wdBorder...}) defining the side of the border.
%%     ~~Describe \code{border} here~~
}
  \item{lty}{a Word constant (\code{wdConst$wdLineStyle...}) defining the line type.
%%     ~~Describe \code{lty} here~~
}
  \item{wrd}{a pointer to the current Word instance.
%%     ~~Describe \code{wrd} here~~
}
}
\value{nothing
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{WrdTable}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{

# create table
tab <- table(op=d.pizza$operator, area=d.pizza$area)

# send it to Word
wrd <- GetNewWrd()
wtab <- ToWrd(tab, wrd=wrd, tablestyle = NA)

# draw borders
WrdTableBorders(wtab, from=c(2,2), to=c(3,3), border=wdConst$wdBorderBottom, wrd=wrd)
WrdTableBorders(wtab, from=c(2,2), to=c(3,3), border=wdConst$wdBorderDiagonalUp, wrd=wrd)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print}
