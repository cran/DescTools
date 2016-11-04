\name{ImportFileDlg}
\alias{ImportFileDlg}

\title{Import File In Foreign Format
}
\description{Import a file in SPSS, Stata, SAS, Minitab or Systat format interactively.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ImportFileDlg(auto_type = TRUE, env = .GlobalEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{auto_type}{determines if the type dialog should be displayed. Default is \code{FALSE}, so that if the file extensions are ".sav", "dta" the appropiate dialog will be called directly.}
\item{env}{environment in which the dataset should be created.}

}
\details{The imported file will be assigned to the given variablename.
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{FileOpenCmd}}
}
\keyword{ manip }
