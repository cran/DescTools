\name{Desc.data.frame}
\alias{Desc.data.frame}
\alias{Desc.list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Describe a data.frame Or a List
%%  ~~function to do ... ~~
}
\description{Describes all the columns of a data.frame, resp. all the elements of a list, according to their class.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{Desc}{data.frame}(x, sep = paste(rep("-", (as.numeric(options("width")) - 2)), collapse = ""), 
     main = NULL, enum = TRUE, ...)

\method{Desc}{list}(x, sep = paste(rep("-", (as.numeric(options("width")) - 2)), collapse = ""), 
     main = NULL, enum = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the data.frame, the matrix or the list to be described
%%     ~~Describe \code{x} here~~
}
  \item{sep}{character. The separator for the title.
%%     ~~Describe \code{sep} here~~
}
\item{main}{a vector with the main titles for the description of the variables. If this is left blank, the
title will be composed as: number - variablename (class(es)).}
\item{enum}{logical, determining if the number should be included or not. Default is TRUE. The numbers may be redundant or inconsistent, if a Word report with enumerated headings is created. }
  \item{\dots}{the dots are passed to the child functions.
%%     ~~Describe \code{\dots} here~~
}
}
\details{See detailed information in the description of the according object interfaces. 
}
\value{No results are returned.
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{Desc}}, \code{\link{PlotDesc}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Desc(d.pizza[,c("temperature","count","driver","date","wine_delivered")] )

Desc(as.list(d.pizza[,c("temperature","count","driver","date","wine_delivered")]) )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }
