\name{XLGetRange}
\alias{XLGetRange}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get the Values of Cell Range(s) in Excel
%%  ~~function to do ... ~~
}
\description{Uses the package \code{RDCOMClient} to open an Excel workbook and return the content of one (or several) given range(s) 
in a specified sheet. Helpful, if pathologically scattered data on an Excel sheet, which can't simply be saved as CSV-file, has to be imported in R.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
XLGetRange(file = NULL, sheet = NULL, range = NULL, as.data.frame = TRUE, 
           header = FALSE, stringsAsFactors = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{the fully specified path and filename of the workbook. If it is left as NULL, the 
  function will look for a running Excel-Application and use its current sheet. The parameter sheet will then be 
  ignored.
%%     ~~Describe \code{fn} here~~
}
  \item{sheet}{the name of the sheet containing the range(s) of interest.
%%     ~~Describe \code{sheet} here~~
}
  \item{range}{a scalar or a vector of the range(s) to be returned (characters). 
  Use "A1"-address mode to specify the ranges, for example \code{"A1:F10"}. \cr If set to NULL (which is the default), the function will look for a selection containing more than one cell. If
  found, the function will use this selection. If there is no selection then the current region of the current selected cell will be used. 
%%     ~~Describe \code{cellrange} here~~
}
\item{as.data.frame}{logical. Determines if the cellranges should be turned into data.frames. Defaults to TRUE, 
as this might be the common use of this function.}
\item{header}{a logical value indicating whether the range contains the names of the variables as its first line. Default is FALSE. \code{header} is ignored if \code{as.data.frame} has been set to FALSE. }

\item{stringsAsFactors}{logical. Should character columns be coerced to factors? Default is FALSE, which will return character vectors.}

}
\details{The result consists of a list of lists, if \code{as.data.frame} is set to \code{FALSE}. 
Be then prepared to encounter \code{NULL} values. Those will prevent from easily being able to coerce
the square data structure to a data.frame.

The following code will replace the \code{NULL} values by NAs and coerce to a data.frame.
\preformatted{
  # get the range D1:J69 from an excel file
  xlrng <- XLGetRange(file="myfile.xlsx", sheet="Tabelle1", 
                      range="D1:J69", as.data.frame=FALSE)

  # replace NULL values by NA
  xlrng[unlist(lapply(xlrng,is.null))] <- NA 

  # coerce the square data structure to a data.frame
  d.lka <- data.frame(lapply(data.frame(xlrng), unlist))
}
This of course can be avoided by setting \code{as.data.frame} to \code{TRUE}.
}
\value{If as.data.frame is set to TRUE, a single data.frame or a list of data.frames will be returned. 
If set to FALSE a list of the cell content in the specified Excel range, resp. a list of lists will be returned.
}
\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{GetNewXL}}, \code{\link{XLGetWorkbook}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{ # Windows-specific example

XLGetRange(file="C:\My Documents\data.xls", 
           sheet="Sheet1", 
           range=c("A2:B5","M6:X23","C4:D40"))


# if the current region is to be read (inkl. a header), place the cursor in the interesting region
# and run:
d.set <- XLGetRange(range=NULL, header=TRUE)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

