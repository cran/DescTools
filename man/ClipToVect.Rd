\name{ClipToVect}
\alias{ClipToVect}

\title{Reformat a Table in the Clipboard as Vector
}
\description{A table in the clipboard can not easily be inserted in the R-code. Though it can be accessed by \code{read.table(clipboard)}, for saving purposes it then has to be saved
by \code{write.table}, which seems clumsy if the file has to be used by others. 
ClipToVect reformats the table in the way that it can be defined as a data.frame and so be pasted directly in the source code.
The option will of course only be interesting for small datasets.
}
\usage{
ClipToVect(doubleQuote = TRUE)
}
\arguments{
  \item{doubleQuote}{logical. Defines if text should be put in doubleQuotes or singleQuotes. Default is double quotes.
}
}
\details{For using this function just copy a cell range in Excel for exmple, then run \code{ClipToVect} and insert the clipboard in the code file.
}
\value{the formatted text is copied to clipboard.
}

\seealso{\code{\link{dput}}
}

\author{Andri Signorell <andri@signorell.net>
}
\keyword{ manip}

