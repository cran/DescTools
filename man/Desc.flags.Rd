\name{Desc.flags}
\alias{Desc.flags}
\alias{Flags}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Describe a Set of Dichotomous Variables
%%  ~~function to do ... ~~
}
\description{Dichotomous variables can easily be condensed in one graphical representation. Desc for a set of flags (=dichotomous variables) calculates the frequencies, a binomial confidence intervall and produces a kind of dotplot with error bars.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{Desc}{flags}(x, i = 1, plotit = getOption("plotit", FALSE), ...)
Flags(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a data.frame
%%     ~~Describe \code{x} here~~
}
  \item{i}{
%%     ~~Describe \code{i} here~~
}
  \item{plotit}{
%%     ~~Describe \code{plotit} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{Motivation for this function is, that dichotomous variable in general do not contain intense information. Therefore it makes sense to condense the description of sets of dichotomous variables.
%%  ~~ If necessary, more details than the description above ~~
}
\value{no results are returned.
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{Desc}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, i = 1, plotit = getOption("plotit", FALSE), ...) 
{
    cat(paste(rep("-", (as.numeric(options("width")) - 2)), collapse = ""), 
        "\n")
    cat("Multiple dichotomous variables")
    if (!is.null(attr(x, "label"))) 
        cat(" :", strwrap(attr(x, "label"), indent = 2, exdent = 2), 
            sep = "\n")
    cat("\n")
    cat("\nSummary: \n", "total n: ", nrow(x), "\n\n", sep = "")
    d.sub <- x
    flags <- do.call(rbind, lapply(d.sub, function(z) {
        tab <- table(z)
        data.frame(val = names(tab[i]), abs = tab[i], BinomCI(tab[i], 
            sum(tab)))
    }))
    out <- data.frame(do.call(rbind, lapply(d.sub, function(x) cbind(NAs = sum(is.na(x)), 
        n = length(x) - sum(is.na(x))))), flags)
    out[, 5:7] <- apply(out[, 5:7], 2, FormatFix, after = 3)
    print(out, quote = FALSE)
    cat("\n")
    if (plotit) 
        PlotDesc(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }

