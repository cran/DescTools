\name{PlotHorizBar}
\alias{PlotHorizBar}


\title{Plot Horizontal Bars}

\description{A more flexible implementation of plotting horizontal bars with definable start and end points.
}
\usage{
PlotHorizBar(from, to, grp = 1, col = "lightgrey", border = "black",
             height = 0.6, add = FALSE, xlim = NULL, ylim = NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{from}{a numeric vector specifying the start of the bars.
%%     ~~Describe \code{from} here~~
}
  \item{to}{a numeric vector specifying the end of the bars.
%%     ~~Describe \code{to} here~~
}
  \item{grp}{a grouping factor, determining on which line the bar will be printed. The groups
  start with y=1 and grow.
%%     ~~Describe \code{grp} here~~
}
  \item{col}{a vector with the colors of the bars. Default is "lightgrey". This will be recyled if necessary.
%%     ~~Describe \code{col} here~~
}
  \item{border}{the border color of the bars. Default ist "black". Set this to NA if no border is to be printed.
%%     ~~Describe \code{col} here~~
}
  \item{height}{the height of the bars. Defaults to 0.6.
%%     ~~Describe \code{col} here~~
}
  \item{add}{logical, if TRUE (default) add bars to current plot.
%%     ~~Describe \code{col} here~~
}
  \item{xlim, ylim}{limits in x and y.
%%     ~~Describe \code{col} here~~
}
  \item{\dots}{the dots are passed to plot.new.
%%     ~~Describe \code{col} here~~
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
%% \value{no
%%}

\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{barplot}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
PlotHorizBar(from=1:5, to=3:7, grp=1:5, col=PalHelsana()[1:5])
}
\keyword{ hplot}
