\name{AddLoess}
\alias{AddLoess}
\alias{AddLoess.default}
\alias{AddLoess.formula}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add a Loess Smoother 
}
\description{
Add a loess smoother to an existing plot. The function first calculates the prediction of a loess object for a reasonable amount of points, then adds the line to the plot and inserts a polygon with the confidence intervals.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{AddLoess}{default}(x, y, ...) 

\method{AddLoess}{formula}(formula, data, col = "#8296C4FF", lwd = 2, lty = "solid", 
                 type = "l", n = 100, conf.level = 0.95, args.band = NULL, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{vectors giving the coordinates of the points in the scatter plot.  
}
  \item{formula}{a formula specifying the numeric response and one to four numeric predictors 
  (best specified via an interaction, but can also be specified additively). Will be coerced to a formula if necessary.
}
  \item{data}{an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) 
   containing the variables in the model. If not found in data, the variables are taken from environment(formula), 
   typically the environment from which loess is called.
}
  \item{col}{linecolor of the smoother. Default is a lightblue color #8296C4FF.
%%     ~~Describe \code{col} here~~
}
  \item{lwd}{line width of the smoother.
%%     ~~Describe \code{lwd} here~~
}
  \item{lty}{line type of the smoother.
%%     ~~Describe \code{lwd} here~~
}
  \item{type}{type of plot, defaults to \code{"l"}.
%%     ~~Describe \code{type} here~~
}
  \item{n}{number of points used for plotting the fit.
%%     ~~Describe \code{type} here~~
}
  \item{conf.level}{confidence level for the confidence interval. Set this to NA, if no confidence band should be plotted.
  Default is 0.95.
%%     ~~Describe \code{type} here~~
}
  \item{args.band}{list of arguments for the confidence band, such as color or border (see \code{\link{DrawBand}}). 
%%     ~~Describe \code{type} here~~
}

  \item{\dots}{further arguments are passed to \code{loess()}
%%     ~~Describe \code{\dots} here~~
}
}
\note{Loess can result in heavy computational load if there are many points!
%%  ~~ If necessary, more details than the description above ~~
}
\value{The computed object of class \kbd{"loess"}. 
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
\code{\link{loess}}, \code{\link{scatter.smooth}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
par(mfrow=c(1,2))

x <- runif(100)
y <- rnorm(100)
plot(x, y)
AddLoess(x, y)

# the formula interface is implemented as well:
data(d.pizza)
plot(temperature ~ delivery_min, data=d.pizza)
AddLoess(temperature ~ delivery_min, data=d.pizza)

plot(temperature ~ delivery_min, data=d.pizza)
AddLoess(temperature ~ delivery_min, data=d.pizza, conf.level = 0.99, 
         args.band = list(col=SetAlpha("red", 0.4), border="black") )

# the default values from scatter.smooth
AddLoess(temperature ~ delivery_min, data=d.pizza, 
         span=2/3, degree=1, family="symmetric", col="red")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
\keyword{ aplot }

