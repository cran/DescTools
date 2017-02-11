\name{TitleRect}
\alias{TitleRect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Boxed Annotation
%%  ~~function to do ... ~~
}
\description{The function can be used to add a title to a plot surrounded by a rectangular box. This is useful for plotting several plots in narrow distances.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
TitleRect(label, bg = "grey", border = 1, col = "black", xjust = 0.5, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{label}{the main title
%%     ~~Describe \code{label} here~~
}
  \item{bg}{the background color of the box.
%%     ~~Describe \code{bg} here~~
}
  \item{border}{the border color of the box
%%     ~~Describe \code{border} here~~
}
  \item{col}{the font color of the title
%%     ~~Describe \code{col} here~~
}
  \item{xjust}{the x-justification of the text. This can be \code{c(0, 0.5, 1)} for left, middle- and right alignement.
%%     ~~Describe \code{xjust} here~~
}
  \item{\dots}{the dots are passed to the \code{\link{text}} function, which can be used to change font and similar arguments.
%%     ~~Describe \code{\dots} here~~
}
}
\value{nothing is returned
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{title}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
plot(pressure)
TitleRect("pressure")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }% __ONLY ONE__ keyword per line
