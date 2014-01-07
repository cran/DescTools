\name{Label}
\alias{Label}
\alias{Label<-}
\alias{Label.default}
\alias{Label<-.default}
\alias{Label.data.frame}
\alias{Label<-.data.frame}

\title{Label Attribute of an Object
%%  ~~function to do ... ~~
}
\description{ Set and retrieve the \code{label} attribute of \code{x}. This can be helpful for documenting the specific meaning of a variable. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Label(x, default = NULL, ...)

\method{Label}{default}(x, ...)

\method{Label}{data.frame}(x, ...)

Label(x, ...) <- value

\method{Label}{default}(x, ...) <- value

\method{Label}{data.frame}(x, self = TRUE, ...) <- value

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{any object
%%     ~~Describe \code{x} here~~
}
 \item{default}{any default
%%     ~~Describe \code{x} here~~
}
  \item{value}{any object
%%     ~~Describe \code{x} here~~
}
  \item{self}{any object
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{the dots are passed to the specific function.
%%     ~~Describe \code{} here~~
}
}

\details{The label should consist of a single text (length of 1). The text may contain any line feeds.
It can be deleted by setting the label to \code{NULL}. 
}
\value{
\code{Label} returns the label attribute of x, if any; otherwise, NULL. 
}

\author{Andri Signorell <andri@signorell.net>
}

\seealso{
A more elaborated version can be found in package \pkg{Hmisc} \code{\link[Hmisc]{label}()}.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

# add a descriptive label to a variable
Label(d.diamonds$colour) <- "The rating scale applied to diamonds ranges from colorless
to yellow, as any other color is extremely rare."

# technically just appending the text as attribute to the variable
attributes(d.diamonds$colour)

# label is supported while describing data
Desc(d.diamonds$colour)

# The label can be deleted by setting it to NULL
Label(d.diamonds$colour) <- NULL

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{misc}
\keyword{utilities}
