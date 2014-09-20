\name{AreaIdent}
\alias{AreaIdent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Identify Points in Plot Lying within a Rectangle or Polygon
%%  ~~function to do ... ~~
}
\description{Find all the points lying either in a rectangle area, spanned by an upper left point and a bottom-right point clicked by the user or in a polygon area defined by the user.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AreaIdent(formula, data, subset, poly = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a \code{\link{formula}}, such as \code{y ~ x} specifying x and y values. \cr 
    Here the formula must be entered that was used to create the scatterplot.
%%     ~~Describe \code{formula} here~~
}
  \item{data}{a data frame (or list) from which the variables in
    \code{formula} should be taken.
%%     ~~Describe \code{data} here~~
}
  \item{subset}{ an optional vector specifying a subset of observations
    to be used.
%%     ~~Describe \code{subset} here~~
}
  \item{poly}{logical. Defines if a polygon or a rectangle should be used to select the points. Default is rectangle.
  If a polygon should be used, set this argument to TRUE and select all desired points. The polygon will be closed 
  automatically when finished.
}
}
\value{Index vector with the points lying within the selected area.
The coordinates are returned as text in the attribute \code{"cond"}.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}


\seealso{\code{\link{identify}}, \code{\link{locator}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{ 
# run the example via copy and paste

plot(temperature ~ delivery_min, data=d.pizza)
idx <- AreaIdent(temperature ~ delivery_min, data=d.pizza)

# you selected the following points
d.pizza[idx,]
points(temperature ~ delivery_min, data = d.pizza[idx,], col="green")

# use the attr("cond") for subsets in code
attr(idx, "cond")

# create a group variable for the found points
d.pizza$grp <- seq(nrow(d.pizza)) \%in\% idx 

# try the polygon option
idx <- AreaIdent(temperature ~ delivery_min, data=d.pizza, poly=TRUE)
points(temperature ~ delivery_min, data = d.pizza[idx,], col="red")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
