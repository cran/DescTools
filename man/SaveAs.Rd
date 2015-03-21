\name{SaveAs}
\alias{SaveAs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Save R Objects By Choosing a File Interactively
%%  ~~function to do ... ~~
}
\description{\code{SaveAs} displays the \code{\link{file.choose}} dialog to get a path/filename and uses \code{\link{save}} to write an external representation of R objects to the specified file
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SaveAs(x, filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the name of the objects to be saved (as symbols or character strings).
%%     ~~Describe \code{x} here~~
}
  \item{filename}{a (writable binary-mode) connection or the name of the file where the data will be saved (when tilde expansion is done). Must be a file name for save.image or version = 1. This is passed directly to the function \code{\link{save}}.
%%     ~~Describe \code{filename} here~~
}
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{save}}, \code{\link{file.choose}}
%% ~~objects to See Also as \code{\link{help}}, ~~~

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip }
