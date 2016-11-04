\name{LineToUser}
\alias{LineToUser}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert Line Coordinates To User Coordinates
%%  ~~function to do ... ~~
}
\description{Convert Line Coordinates To User Coordinates
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
LineToUser(line, side)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{line}{the number of lines
%%     ~~Describe \code{line} here~~
}
  \item{side}{the side of the plot
%%     ~~Describe \code{side} here~~
}
}
\details{Not trivial
%%  ~~ If necessary, more details than the description above ~~
}
\value{user coordinates
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{mtext}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
plot(1:10)
LineToUser(line=2, side=4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }% use one of  RShowDoc("KEYWORDS")
