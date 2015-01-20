\name{DescWrd}

\alias{DescWrd}
\alias{DescWrd.data.frame}
\alias{DescWrd.list}
\alias{DescWrd.default}
\alias{DescWrd.formula}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Use Word as Reporting Tool for Describing Data
%%  ~~function to do ... ~~
}
\description{
Calculates descriptive statistics for x and uses Word as reporting tool.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{

\method{DescWrd}{default}(x, wrd, main = deparse(substitute(x)), \dots)

\method{DescWrd}{data.frame}(x, wrd, main = NULL, enum = TRUE, \dots)

\method{DescWrd}{list}(x, wrd, main = NULL, enum = TRUE, \dots)

\method{DescWrd}{formula}(formula, data = parent.frame(), subset = TRUE, wrd, \dots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the object to be described.}
\item{main}{the title of the decription or a vector with the main titles for the description of the variables. If this is left blank, the
title will be composed as: number - variablename (class(es)).}
\item{enum}{logical, determining if the number should be included or not. Default is TRUE. The numbers may be redundant or inconsistent, if a Word report with enumerated headings is created. }
  \item{wrd}{the pointer to a word instance. Can be a new one, created by \code{GetNewWrd()} 
  or an existing one, created by \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{getOption("lastWord")}.}

  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the data values and rhs the corresponding groups.}  

  \item{data}{an optional matrix or data frame containing the variables in the formula \code{formula}. 
    By default the variables are taken from \code{environment(formula)}.}  

  \item{subset}{an optional vector specifying a subset of observations to be used.}  
  
  \item{\dots}{further argument to be passed to methods.}

}

\details{This function is not thought of being directly run by the enduser. It will normally be called automatically, when 
a pointer to a Word instance is passed to the function \code{\link{Desc}}.\cr 
However \code{DescWrd} takes some more specific arguments concerning the Word output (like font or fontsize), which can make it necessary to call the function directly.
%%  ~~ If necessary, more details than the description above ~~
}
\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{ \code{\link{Desc}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Output into word document ---------------------------------------------------------

\dontrun{# Windows-specific example

wrd <- GetNewWrd(header=TRUE)    # create a new word instance and insert title and contents

# let's have a subset
d.sub <- d.pizza[,c("driver","date","operator","price","wrongpizza")]

# do just the univariate analysis
Desc(d.sub, wrd=wrd) 

# do a full report
Desc(d.sub, wrd=wrd, colpairs=TRUE)

# do just bivariate analysis
Desc( d.sub, univar=FALSE, colpairs=TRUE, wrd=wrd) 

# selected bivariate analysis into word document
Desc(week ~ driver, data=d.pizza, wrd=wrd)   
Desc(price ~ operator, data=d.pizza, digits=c(2,2,2,2,0,3,0,0), wrd=wrd ) 
Desc(driver ~ operator, data=d.pizza, wrd=wrd) 
Desc(price ~ operator + driver + wrongpizza, data=d.pizza
     , digits=c(2,2,2,2,0,3,0,0), wrd=wrd) 

Desc(price ~ delivery_min, data=d.pizza, wrd=wrd ) 


# internal functions (not meant to be used by the enduser):
Desc.factor(d.pizza$driver, ord="n" )  # ordered by name
Desc.factor(d.pizza$driver, ord="l" )  # ordered by level
Desc.logical(d.pizza$wrongpizza)
Desc.integer(d.pizza$weekday, maxlevels=NA)
Desc.integer(d.pizza$weekday, maxlevels=3)
Desc.numeric(d.pizza$count, highlow=FALSE)
Desc.numeric(d.pizza$count, highlow=TRUE)
 
DescNumFact( x=d.pizza$delivery_min, grp=d.pizza$operator )
DescFactFact( x=d.pizza$driver, grp=d.pizza$operator)
DescNumNum( x=d.pizza$delivery_min, y=d.pizza$price )
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }
\keyword{ IO }