
\name{PlotDot}
\alias{PlotDot}
\title{Cleveland's Dot Plots}
\description{
  Draw a Cleveland dot plot. This is an extended version of \code{\link{dotchart}} with an added option for error bars and an \code{add} argument.
}
\usage{
PlotDot(x, labels = NULL, groups = NULL, gdata = NULL,
        cex = par("cex"), pch = 21, gpch = 21, bg = par("bg"),
        color = par("fg"), gcolor = par("fg"), lcolor = "gray",
        xlim = NULL, main = NULL, xlab = NULL, ylab = NULL, 
        add = FALSE, args.errbars = NULL, \dots)
}
\arguments{
  \item{x}{either a vector or matrix of numeric values (\code{NA}s are
    allowed).  If \code{x} is a matrix the overall plot consists of
    juxtaposed dotplots for each row.  Inputs which satisfy
    \code{\link{is.numeric}(x)} but not
    \code{is.vector(x) || is.matrix(x)} are coerced by
    \code{\link{as.numeric}}, with a warning.}
  \item{labels}{a vector of labels for each point.
    For vectors the default is to use \code{names(x)} and for matrices
    the row labels \code{dimnames(x)[[1]]}.}
  \item{groups}{an optional factor indicating how the elements of
    \code{x} are grouped.
    If \code{x} is a matrix, \code{groups} will default to the columns
    of \code{x}.}
  \item{gdata}{data values for the groups.
    This is typically a summary such as the median or mean
    of each group.}
  \item{cex}{the character size to be used.  Setting \code{cex}
    to a value smaller than one can be a useful way of avoiding label
    overlap.  Unlike many other graphics functions, this sets the actual
    size, not a multiple of \code{par("cex")}.}
  \item{pch}{the plotting character or symbol to be used.}
  \item{gpch}{the plotting character or symbol to be used for group
    values.}
  \item{bg}{the background color of plotting characters or symbols to be
    used; use \code{\link{par}(bg= *)} to set the background color of
    the whole plot.}
  \item{color}{the color(s) to be used for points and labels.}
  \item{gcolor}{the single color to be used for group labels and
    values.}
  \item{lcolor}{the color(s) to be used for the horizontal lines.}
  \item{xlim}{horizontal range for the plot, see
    \code{\link{plot.window}}, e.g.}
  \item{main}{overall title for the plot, see \code{\link{title}}.}
  \item{xlab, ylab}{axis annotations as in \code{title}.}
  \item{add}{logical specifying if bars should be added to an already existing plot; defaults to \code{FALSE}.}
  \item{args.errbars}{optional arguments for adding error bars. All arguments for \code{\link{ErrBars}} can be supplied. If left to NULL (default), no error bars will be plotted.}
  \item{\dots}{\link{graphical parameters} can also be specified as arguments.}
}
\details{
  This function is invoked for its side effect, which is to produce two
  variants of dotplots as described in Cleveland (1985).

  Dot plots are a reasonable substitute for bar plots.
}
\value{Return the y-values used for plotting.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Cleveland, W. S. (1985)
  \emph{The Elements of Graphing Data.}
  Monterey, CA: Wadsworth.

  Murrell, P. (2005) \emph{R Graphics}. Chapman & Hall/CRC Press.
}
\author{R-Core with some extensions by Andri Signorell <andri@signorell.net>}
\examples{
PlotDot(VADeaths, main = "Death Rates in Virginia - 1940")
op <- par(xaxs = "i")  # 0 -- 100\%
PlotDot(t(VADeaths), xlim = c(0,100),
         main = "Death Rates in Virginia - 1940")
par(op)

# add some error bars
PlotDot(VADeaths, main="Death Rates in Virginia - 1940", col="red", pch=NA, 
        args.errbars = list(from=VADeaths-2, to=VADeaths+2, mid=VADeaths, 
        pch=21, cex=1.4))
        
# add some other values
PlotDot(VADeaths+3, pch=15, col="blue", add=TRUE, labels=NA)

# same as PlotDotCI
xci <- do.call(rbind, tapply( d.pizza$delivery_min, d.pizza$driver, 
               MeanCI, conf.level=0.99, na.rm=TRUE))

PlotDot(xci[,1], main="delivery_min ~ driver", 
        args.errbars = list(from=xci[,2], to=xci[,3], mid=xci[,1], 
        lwd=2, pch=21, col="grey40", bg.pch="grey50", col.pch="black"),
        xlim=c(15,35), panel.before=grid())

# with group data
x <- with(d.pizza, tapply(temperature, list(area, driver), mean, na.rm=TRUE))

PlotDot(x, gdata = tapply(d.pizza$temperature, d.pizza$driver, mean, na.rm=TRUE),
        gpch = 15)
        
# special format
par(lend=1)

PlotDot(VADeaths, main="Death Rates in Virginia - 1940", pch=NA, lcolor = hecru,
        args.errbars = list(from=VADeaths-2, to=VADeaths+2, mid=VADeaths, 
                            pch="|", col.pch=hred, cex=1.3, lwd=8, code=0, col=hgreen))
}
\keyword{hplot}


