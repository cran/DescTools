\name{GetNewWrd}
\alias{GetNewWrd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a new Word Instance
%%  ~~function to do ... ~~
}
\description{Start a new instance of Word and return its handle. 
This handle allows controlling word afterwards.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GetNewWrd(visible = TRUE, template = "Normal", header = FALSE, 
          main = "Descriptive report")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{visible}{logical, should Word made visible? Defaults to TRUE.
%%     ~~Describe \code{visible} here~~
}
  \item{template}{the name of the template to be used for creating a new document.
%%     ~~Describe \code{template} here~~
}
  \item{header}{logical, should a caption and a list of contents be inserted? Default is FALSE.
%%     ~~Describe \code{header} here~~
}
  \item{main}{the main title of the report
%%     ~~Describe \code{main} here~~
}

}
\details{
Note that the list of contents has to be refreshed by hand after inserting text (if inserted by \code{header = TRUE}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{a handle (pointer) to the created Word instance. 
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Andri Signorell <andri@signorell.net>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{GetNewXL}}, \code{\link{GetNewPP}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{ # Windows-specific example

wrd <- GetNewWrd()
Desc(d.pizza[,1:4], wrd=wrd)

wrd <- GetNewWrd(header=TRUE)
Desc(d.pizza[,1:4], wrd=wrd)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }

