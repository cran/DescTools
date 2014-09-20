\name{FormatSig}
\alias{FormatSig}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Significance Representation of a P-Value
%%  ~~function to do ... ~~
}
\description{Create a significance representation of a p-value consisting of * and . according to the used defaults e.g. in \code{lm}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
FormatSig(x, breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
          labels = c("***", "** ", "*  ", ".  ", "   "))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numerical vector.
%%     ~~Describe \code{x} here~~
}
  \item{breaks}{the breaks for the significance groups.
%%     ~~Describe \code{breaks} here~~
}
  \item{labels}{the labels for the significance groups.
%%     ~~Describe \code{labels} here~~
}
}
\value{The string representing the significance format of \code{x}.
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{FormatFix}}, \code{\link{format}}
%% ~~objects to See Also as , ~~~
}
\examples{
FormatSig(x=c(0.12, 0.07,0.03, 0.008, 0.000001))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print }
