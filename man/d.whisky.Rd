\name{d.whisky}
\alias{d.whisky}
\docType{data}
\title{Classification of Scotch Single Malts
%%   ~~ data name/kind ... ~~
}
\description{86 malt whiskies are scored between 0-4 for 12 different taste categories including sweetness, smoky, nutty etc. Additionally, coordinates of distilleries allow us to obtain pairwise distance information. Using a combination of these variables it is possible to look for correlations between particular attributes of taste and physical location, for example does a shared local resource have a significant effect on nearby whiskies.\cr
By using correlation analysis it may be possible to provide whisky recommendations based upon an individual's particular preferences.
By computing the Pearson correlation coefficient and specifying a threshold value between 0 and 1, we can establish an adjacency matrix where each node is a malt whisky and an edge represents a level of similarity above the threshold.
}
\usage{data("d.whisky")}
\format{
  A data frame with 86 observations on the following 16 variables.
  \describe{
    \item{\code{distillery}}{a factor with levels \code{Aberfeldy}, \code{Aberlour}, \code{AnCnoc}, \code{Ardbeg}, ... }
    \item{\code{body}}{a numeric vector}
    \item{\code{sweetness}}{a numeric vector}
    \item{\code{smoky}}{a numeric vector}
    \item{\code{medicinal}}{a numeric vector}
    \item{\code{tobacco}}{a numeric vector}
    \item{\code{honey}}{a numeric vector}
    \item{\code{spicy}}{a numeric vector}
    \item{\code{winey}}{a numeric vector}
    \item{\code{nutty}}{a numeric vector}
    \item{\code{malty}}{a numeric vector}
    \item{\code{fruity}}{a numeric vector}
    \item{\code{floral}}{a numeric vector}
    \item{\code{postcode}}{a factor with levels \code{ AB30 1YE}, \code{ AB35 5TB}, ... }
    \item{\code{latitude}}{a numeric vector, coordinate pairs of distilleries.}
    \item{\code{longitude}}{a numeric vector, coordinate pairs of distilleries.}
  }
}
\source{\url{http://www.mathstat.strath.ac.uk/outreach/nessie/nessie_whisky.html}
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
\url{http://www.whiskyclassified.com}\cr
\url{http://www.mathstat.strath.ac.uk/outreach/nessie/index.html}
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
head(d.whisky)
}
\keyword{datasets}
