\name{DrawBand}
\alias{DrawBand}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draw Confidence Band
%%  ~~function to do ... ~~
}
\description{Draw a (confidence) band. Just a wrapper for polygon.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DrawBand(x, y, col = SetAlpha("grey", 0.5), border = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector with x coordinates for the band.
%%     ~~Describe \code{x} here~~
}
  \item{y}{a vector with y coordinates for the band.
%%     ~~Describe \code{y} here~~
}
  \item{col}{the color of the band.
%%     ~~Describe \code{col} here~~
}
  \item{border}{the border color of the band.
%%     ~~Describe \code{border} here~~
}
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{polygon}}
%% ~~objects to See Also as , ~~~
}
\examples{
set.seed(18)
x <- rnorm(15)
y <- x + rnorm(15)

new <- seq(-3, 3, 0.5)
pred.w.plim <- predict(lm(y ~ x), newdata=data.frame(x=new), interval="prediction")
pred.w.clim <- predict(lm(y ~ x), newdata=data.frame(x=new), interval="confidence")

plot(y ~ x)
DrawBand(y = c(pred.w.plim[,2], rev(pred.w.plim[,3])), 
  x=c(new, rev(new)), col= SetAlpha("grey90", 0.5))
DrawBand(y = c(pred.w.clim[,2], rev(pred.w.clim[,3])), 
  x=c(new, rev(new)), col= SetAlpha("grey80", 0.5))

abline(lm(y~x), col="brown")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
