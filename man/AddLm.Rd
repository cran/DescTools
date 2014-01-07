\name{AddLm}
\alias{AddLm}
\alias{AddLm.default}
\alias{AddLm.formula}
\alias{AddLm.lm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add a Linear Regression Line
%%  ~~function to do ... ~~
}
\description{
Add a linear regression line to an existing plot. The function first calculates the prediction of a lm object for a reasonable amount of points, then adds the line to the plot and inserts a polygon with the confidence and prediction intervals.
}
\usage{
\method{AddLm}{default}(x, y, ...) 
\method{AddLm}{formula}(formula, data, ...) 
\method{AddLm}{lm}(x, col = "blue", lwd = 2, lty = "solid", 
         type = "l", n = 100, conf.level = 0.95, args.cband = NULL, 
         pred.level = NA, args.pband = NULL, ...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{vectors giving the coordinates of the points in the scatter plot.  
}
  \item{formula}{a formula specifying the numeric response and one numeric predictor. Will be coerced to a formula if necessary.
}
  \item{data}{an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) 
   containing the variables in the model. If not found in data, the variables are taken from environment(formula), 
   typically the environment from which the function is called.
}
  \item{col}{linecolor of the line.
%%     ~~Describe \code{col} here~~
}
  \item{lwd}{line width of the line.
%%     ~~Describe \code{lwd} here~~
}
  \item{lty}{line type of the line.
%%     ~~Describe \code{lwd} here~~
}
  \item{type}{character indicating the type of plotting; actually any of the \code{types} as in \code{\link{plot.default}}. Type of plot, defaults to \code{"l"}.
%%     ~~Describe \code{type} here~~
}
  \item{n}{number of points used for plotting the fit.
%%     ~~Describe \code{type} here~~
}
  \item{conf.level}{confidence level for the confidence interval. Set this to \code{NA}, if no confidence band should be plotted.
  Default is \code{0.95}.
%%     ~~Describe \code{type} here~~
}
  \item{args.cband}{list of arguments for the confidence band, such as color or border (see \code{\link{DrawBand}}). 
%%     ~~Describe \code{type} here~~
}
  \item{pred.level}{confidence level for the prediction interval. Set this to NA, if no prediction band should be plotted.
  Default is \code{0.95}.
%%     ~~Describe \code{type} here~~
}
  \item{args.pband}{list of arguments for the prediction band, such as color or border (see \code{\link{DrawBand}}). 
%%     ~~Describe \code{type} here~~
}

  \item{\dots}{further arguments are not used specifically.
%%     ~~Describe \code{\dots} here~~
}
}
\details{
It's sometimes illuminating to plot a regression line with it's prediction, resp. confidence intervals over an existing xy-plot. This only makes sense, if just a simple regression model y ~ x is to be visualized.
%%  ~~ If necessary, more details than the description above ~~
}
\value{nothing 
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{
\code{\link{AddLoess}}, \code{\link{lm}}  
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
par(mfrow=c(1,2))

plot(hp ~ wt, mtcars)
AddLm(hp ~ wt, mtcars, col="steelblue")

# add the prediction intervals in different color
plot(hp ~ wt, mtcars)
AddLm(hp ~ wt, mtcars, col="red", pred.level=0.95, args.pband=list(col=SetAlpha("grey",0.3)) )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
\keyword{math}
