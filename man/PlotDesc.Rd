\name{PlotDesc}
\alias{PlotDesc}
\alias{PlotDesc.default}
\alias{PlotDesc.integer}
\alias{PlotDesc.numeric}
\alias{PlotDesc.factor}
\alias{PlotDesc.table}
\alias{PlotDesc.matrix}
\alias{PlotDesc.ordered}
\alias{PlotDesc.data.frame}
\alias{PlotDesc.logical}
\alias{PlotDesc.Date}
\alias{PlotDesc.flags}

\alias{PlotDescFactFact}
\alias{PlotDescNumFact}
\alias{PlotDescFactNum}
\alias{PlotDescNumNum}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Display descriptive plots
%%  ~~function to do ... ~~
}
\description{
Specific descriptive plots depending on the class of x. These will typically be called by the Desc routines with the plotit argument set to TRUE.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{

PlotDesc(x, ..., wrd = NULL)

\method{PlotDesc}{default}(x, ...)

\method{PlotDesc}{integer}(x, main = deparse(substitute(x)),
         ord = c("val_asc","val_desc","frq_asc","frq_desc"),
         maxrows = 12, ... , wrd = NULL)

\method{PlotDesc}{numeric}(x, main = deparse(substitute(x)), ...,
         wrd = NULL)

\method{PlotDesc}{factor}(x, main = deparse(substitute(x)),
         ord = c("desc", "level", "name", "asc", "none"),
         maxrows = 12, lablen = 25, type = c("bar","dot"),
         col = NULL, border = NULL, xlim=NULL, ecdf=FALSE, ..., wrd = NULL)

\method{PlotDesc}{table}(x, col1 = getOption("col1", hblue),
         col2 = getOption("col2", hred),
         horiz = TRUE,  main="", ..., wrd = NULL)
\method{PlotDesc}{matrix}(x, col1 = getOption("col1", hblue),
         col2 = getOption("col2", hred),
         horiz = TRUE,  main="", ..., wrd = NULL)

\method{PlotDesc}{ordered}(x, ..., wrd = NULL)

\method{PlotDesc}{data.frame}(x, ..., wrd = NULL)

\method{PlotDesc}{logical}(x, main = deparse(substitute(x)), xlab = "",
        col1 = getOption("col1", hblue),
        col2 = getOption("col2", hred), ..., wrd = NULL)

\method{PlotDesc}{Date}(x, main = deparse(substitute(x)), breaks = NULL, ..., wrd = NULL)

\method{PlotDesc}{flags}(x, ..., wrd = NULL)

PlotDescNumFact(formula, data, main = deparse(formula), notch=FALSE,
                add_ni = TRUE, ..., wrd = NULL)

PlotDescFactNum(x, y, ptab,
                col1 = getOption("col1", hblue), col2 = getOption("col2", hred),
                main=NULL, notch=FALSE, add_ni = TRUE, ... , wrd=NULL)

PlotDescNumNum(form1, form2, data, main = NULL, xlab = NULL, ylab = NULL, ..., wrd = NULL)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the vector to be plotted.}
  \item{main}{the main title of the plot.}
  \item{ord}{the row order of a frequency table to be chosen. Is used for factors and integers.}
  \item{maxrows}{the maximum number of rows to be displayed for a factor. }
  \item{lablen}{the maximum numbe of characters for a factor level to be displayed, before the level is truncated and ... are added. }
  \item{type}{the type of plot to be used for describing factors. Can be \code{"bar"} for a horizontal barchart or \code{"dot"} for a dotchart. }
  \item{col1, col2}{two colors to be chosen for doing mosaicplots and logic plots. }
  \item{col}{color of the points used in a dotchart, typically for integers. Defaults to lightblue.}
  \item{border}{the color of the border, if the plottype is barplot or dotplot}
  \item{xlab}{the label for the x-axis.}
  \item{ylab}{the label for the y-axis.}
  \item{xlim}{the limits of the x-axis in factor plots.}
  \item{ecdf}{logical, defining if cumulative frequencies should be displayed in PlotDesc.factor. Defaults to FALSE.}
  \item{breaks}{vector of limits to bin a date.}
  \item{horiz}{logical, indicating if the two mosaicplots should be arranged horizontally (default is \code{TRUE}).}

  \item{formula}{a formula, such as y ~ grp, where y is a numeric vector of data values to be split into groups according to the grouping variable grp (usually a factor). }
  \item{data}{a data.frame (or list) from which the variables in formula should be taken.}
  \item{notch}{if notch is TRUE, a notch is drawn in each side of the boxes. If the notches of two plots do not overlap this is 'strong evidence' that the two medians differ.
  }
  \item{add_ni}{logical. Indicates if the group length should be displayed in the boxplot.}
  \item{form1, form2}{ the formula used for calculating the smoother.}
  \item{wrd}{the pointer to a word instance. Can be a new one, created by \code{GetNewWrd()}
  or an existing one, created by \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{getOption("lastWord")}.}
  \item{y}{the response variable}
  \item{ptab}{the proportions table for the deciles of x used in PlotDesc.flags.}
  \item{\dots}{further arguments
}
}
\details{See the detailed description for informations about specific plots.
}

\value{no value returned
}


\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{Desc}}
}

\examples{
PlotDesc(x=na.omit(d.pizza$delivery_min))    # numeric
PlotDesc(x=na.omit(d.pizza$week))            # integer
PlotDesc(x=na.omit(d.pizza$driver))          # factor
PlotDesc(x=na.omit(d.pizza$quality))         # ordered factor
PlotDesc(x=na.omit(d.pizza$wrongpizza))      # logical
PlotDesc(x=na.omit(d.pizza$date))            # Date

PlotDesc(x=na.omit(d.pizza$driver), ecdf = TRUE)          # factor

d.frm <- d.pizza[,c("price","operator")]
d.frm <- d.frm[complete.cases(d.frm),]
PlotDescNumFact(temperature ~ driver, data=d.pizza)       # numeric ~ factor

PlotDesc(table(d.pizza$driver, d.pizza$operator))         # factor ~ factor
}
\keyword{ hplot}

