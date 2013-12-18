\name{ImportDlg}
\alias{ImportDlg}
\title{Get Path of a Data File to Be Opened
}
\description{Handling of pathnames is tedious in Windows because of the backslashes, that prevent simple pasting of a copied path into the source code. \code{ImportDlg} displays the FileOpen-Dialog for picking a file interactively. When done backslashes in the path are replaced by slashes and the result is being copied into the clipboard, from where it can easily be pasted in any code editor.
}
\usage{
ImportDlg(fmt = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fmt}{the format, in which the filename parts should be returned. \cr 
    Default ist path\\filename.ext and coded as "\"\%path\%\\\%fname\%.\%fxt\%\"". 
    See examples for time saving alternative definitions.
    
}
}
\value{\code{ImportDlg()} invisibly returns the path of the chosen file in the defined format. 
  The result is additionally being copied to the clipboard.
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{file.choose}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

\dontrun{ 
# choose a file
fn <- ImportDlg()
print(gettextf("You chose the file: \%s ", fn))


# the path and filename can as well be nested in a command,
#   done here to build a read.table command that can be well inserted into the code:
ImportDlg(fmt="d.\%fname\% <- read.table(file = \"\%path\%\\\%fname\%.\%fxt\%\", 
  header = TRUE, sep = \";\", na.strings = c(\"NA\",\"NULL\"), strip.white = TRUE)")

# go to your editor and paste...



}


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
