\name{PlotPch}
\alias{PlotPar}
\alias{PlotPch}
\alias{PlotMar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Information plots
%%  ~~function to do ... ~~
}
\description{The function \code{PlotPar()} plots the typically used parameters and their values. \cr
}
\usage{
PlotPar()
PlotPch(col = NULL, bg = NULL, newwin = FALSE)
PlotMar()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{col}{the default plotting color for \code{PlotPch}. Defaults to \code{hred}.}
  \item{bg}{The color to be used for the background of the plot symbols. Default is \code{hecru}.}
  \item{newwin}{logical, defining if a new graphic device should be used. Default is \code{FALSE}.}
}
\details{\code{PlotPch()} will display the plot characters as defined by \code{pch} in all plots in a dialog.

\code{PlotMar()} plots the margins.
%%  ~~ If necessary, more details than the description above ~~
}
\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{par}}, \code{\link{colors}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
PlotPar()
PlotPch()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }

