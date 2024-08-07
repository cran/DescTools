\name{BarText}
\alias{BarText}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Place Value Labels on a Barplot
%%  ~~function to do ... ~~
}
\description{It can sometimes make sense to display data values directly on the bars in a barplot. There are a handful of obvious alternatives for placing the labels, either on top of the bars, right below the upper end, in the middle or at the bottom. Determining the required geometry - although not difficult - is cumbersome and the code is distractingly long within an analysis code. The present function offers a short way to solve the task. It can place text either in the middle of the stacked bars, on top or on the bottom of a barplot (side by side or stacked). 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BarText(height, b, labels = height, beside = FALSE, horiz = FALSE,
        cex = par("cex"), adj = NULL, 
        pos = c("topout", "topin", "mid", "bottomin", "bottomout"), 
        offset = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{height}{either a vector or matrix of values describing the bars which make up the plot exactly as used for creating the barplot.
%%     ~~Describe \code{height} here~~
}
  \item{b}{the returned mid points as returned by \code{b <- barplot(...)}.
%%     ~~Describe \code{b} here~~
}
  \item{labels}{the labels to be placed on the bars.
%%     ~~Describe \code{labels} here~~
}
  \item{beside}{a logical value. If \code{FALSE}, the columns of height are portrayed as stacked bars, and if \code{TRUE} the columns are portrayed as juxtaposed bars.
%%     ~~Describe \code{beside} here~~
}
  \item{horiz}{a logical value. If \code{FALSE}, the bars are drawn vertically with the first bar to the left. If \code{TRUE}, the bars are drawn horizontally with the first at the bottom.
%%     ~~Describe \code{horiz} here~~
}
\item{cex}{numeric character expansion factor; multiplied by \code{\link{par}}\code{("cex")} yields the final character size. \code{NULL} and \code{NA} are equivalent to \code{1.0}.}
\item{adj}{one or two values in [0, 1] which specify the x (and optionally y) adjustment of the labels. On most devices values outside that interval will also work.}

\item{pos}{one of \code{"topout"}, \code{"topin"}, \code{"mid"}, \code{"bottomin"}, \code{"bottomout"}, 
defining if the labels should be placed on top of the bars (inside or outside) or at the bottom of the bars (inside or outside).}

\item{offset}{a vector indicating how much the bars should be shifted relative to the x axis.}

\item{\dots}{the dots are passed to the \code{\link{BoxedText}}.
%%     ~~Describe \code{\dots} here~~
}
}
\details{
The x coordinates of the labels can be found by using \code{\link{barplot}()} result, if they are to be centered at the top of each bar. \code{BarText()} calculates the rest.

\figure{bartext.png}{Positions for the text}

Notice that when the labels are placed on top of the bars, they may be clipped. This can be avoided by setting \code{xpd=TRUE}.
}
\author{Andri Signorell <andri@signorell.net>
}


\value{returns the geometry of the labels invisibly}


\seealso{\code{\link{BoxedText}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# simple vector
x <- c(353, 44, 56, 34)
b <- barplot(x)
BarText(x, b, x)

# more complicated
b <- barplot(VADeaths, horiz = FALSE, col=DescTools::hblue, beside = TRUE)
BarText(VADeaths, b=b, horiz = FALSE, beside = TRUE, cex=0.8)
BarText(VADeaths, b=b, horiz = FALSE, beside = TRUE, cex=0.8, pos="bottomin",
        col="white", font=2)

b <- barplot(VADeaths, horiz = TRUE, col=DescTools::hblue, beside = TRUE)
BarText(VADeaths, b=b, horiz = TRUE, beside = TRUE, cex=0.8)

b <- barplot(VADeaths)
BarText(VADeaths, b=b)

b <- barplot(VADeaths, horiz = TRUE)
BarText(VADeaths, b=b, horiz = TRUE, col="red", cex=1.5)


# position of the text
old <- par(mfrow=c(3,2), xpd=NA)
off <- c(10, 4, 1, 20, -15)

for(pos in eval(formals(BarText)$pos)) {
  b <- barplot(x, offset=off, 
  main=gettextf("Textposition pos = '\%s'", pos), horiz=TRUE)
  abline(h=0)
  BarText(x, b, x, offset = off, pos=pos, cex=1.5, horiz=TRUE)
}
par(old)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }% use one of  RShowDoc("KEYWORDS")
