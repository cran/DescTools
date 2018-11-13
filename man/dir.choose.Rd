\name{dir.choose}
\alias{dir.choose}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Choose a Folder Interactively
%%  ~~function to do ... ~~
}
\description{Choose a directory interactively.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dir.choose(default = "", caption = "Select directory")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{default}{which directory to show initially.
%%     ~~Describe \code{default} here~~
}
  \item{caption}{the caption on the selection dialog
%%     ~~Describe \code{caption} here~~
}
}
\value{A length-one character vector, character \code{NA} if 'Cancel' was selected.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>}
%%  ~~who you are~~
\seealso{\code{\link{file.choose}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{

dir.choose(getwd(), "Choose a suitable folder")
}}
\keyword{file }% __ONLY ONE__ keyword per line
