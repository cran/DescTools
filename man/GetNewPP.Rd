\name{GetNewPP}
\alias{GetNewPP}
\alias{GetCurrPP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a New PowerPoint Instance
%%  ~~function to do ... ~~
}
\description{Start a new instance of PowerPoint and return its handle. A new presentation with one empty slide will be created.
The handle is needed for addressing the presentation afterwards.\cr
GetCurrPP will look for a running PowerPoint instance and return its handle. \code{NULL} is returned if nothing's found.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GetNewPP(visible = TRUE, template = "Normal")

GetCurrPP()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{visible}{logical, should PowerPoint made visible? Defaults to TRUE.
%%     ~~Describe \code{visible} here~~
}
  \item{template}{the name of the template to be used for creating a new presentation.
%%     ~~Describe \code{template} here~~
}
}
\value{a handle (pointer) to the created PowerPoint instance.
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{GetNewXL}}, \code{\link{GetNewWrd}}, \code{\link{PpPlot}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{ # Windows-specific example
# get a handle to a new PowerPoint instance
# (this will be used further to export R-Objects to PowerPoint)
pp <- GetNewPP()
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
