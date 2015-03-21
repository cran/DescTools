\name{Desc}
\alias{Desc}
\alias{Desc.default}

\title{Describe Data}
\description{
Produce summaries of various types of variables. Calculate descriptive statistics for x and use Word as reporting tool for the numeric results and for descriptive plots.
The appropriate statistics are chosen depending on the class of x.
The general intention is to simplify the description process for lazy typers and return a quick, but rich summary.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Desc(x, ..., wrd = NULL)
\method{Desc}{default}(x, \dots)
}
% \method{Desc}{logical}(x, main = deparse(substitute(x)), digits = 3, conf.level = 0.95)
% \method{Desc}{factor}(x, main=deparse(substitute(x)), ord=c("desc","asc","name","level"), maxrows=10, digits=3)
% \method{Desc}{ordered}(x, \dots)
% \method{Desc}{integer}(x, main=deparse(substitute(x)), maxrows=10, maxlevels=10, digits=3, width=7)
% \method{Desc}{numeric}(x, main=deparse(substitute(x)), digits=3, highlow=TRUE)
% \method{Desc}{Date}(x, main=deparse(substitute(x)), maxrows=10, digits=3)
% \method{Desc}{table}(x, main=deparse(substitute(x)))

% \method{Desc}{data.frame}(x, sep=paste(rep("-",110), collapse=""), \dots)
% \method{Desc}{list}(x, \dots)
% \method{Desc}{formula}(x, \dots)

% \method{Desc}{factor.factor}(x, grp, rfrq="100", show.mutinf=FALSE, main=deparse(substitute(x)),
%       grpname=deparse(substitute(grp)))
% \method{Desc}{numeric.factor}(x, grp, digits=c(NA,NA,NA,NA,0,3,0,0), width=getOption("width"), 
%       use.na = c("no", "ifany", "always") )
% \method{Desc}{numeric.numeric}(x, y, main=deparse(substitute(x)), yname=deparse(substitute(y)))


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the object to be described.}
  \item{wrd}{a pointer to a running Word instance. If this is NULL then output will be directed to console, else to Word.}
  \item{\dots}{the dots are passed to the specific function.}
  
}
\details{
Desc is a generic function. It dispatches to one of the methods above depending on the class of its first argument. Typing ?Desc
+ TAB at the prompt should present a choice of links: the help pages for each of these Desc methods (at least if you're using RStudio, which anyway is recommended).
You don't need to use the full name of the method although you may if you wish; i.e.,
Desc(x) is idiomatic R but you can bypass method dispatch by going direct if you wish:
Desc.numeric(x).
More details about the results of the methods can be found in the type-specific help texts.
}
\value{partly results are returned}
\author{Andri Signorell}

\seealso{See the specific object methods:\cr
\code{\link{Desc.logical}}, \code{\link{Desc.factor}}, \code{\link{Desc.ordered}}, \code{\link{Desc.integer}},
\code{\link{Desc.numeric}}, \code{\link{Desc.Date}}, \code{\link{Desc.table}}, \code{\link{Desc.data.frame}},  
\code{\link{Desc.list}}, \code{\link{Desc.formula}}, 
\code{\link{DescFactFact}}, \code{\link{DescNumFact}}, \code{\link{DescNumNum}}
}
\examples{
# implemented classes:
Desc(d.pizza$wrongpizza)     # logical
Desc(d.pizza$driver)         # factor
Desc(d.pizza$quality)        # ordered factor
Desc(d.pizza$week)           # integer
Desc(d.pizza$delivery_min)   # numeric
Desc(d.pizza$date)           # Date

Desc(d.pizza$wrongpizza, main="The wrong pizza delivered", digits=5)  

    
# just selected bivariate analysis on the console
Desc(price ~ operator, data=d.pizza)                  # numeric ~ factor
Desc(driver ~ operator, data=d.pizza)                 # factor ~ factor 
Desc(driver ~ area + operator, data=d.pizza)          # factor ~ several factors
Desc(driver + area ~ operator, data=d.pizza)          # several factors ~ factor
Desc(driver ~ week, data=d.pizza)                     # factor ~ integer 

Desc(driver ~ operator, data=d.pizza, rfrq=("111"))   # alle rel. frequencies
Desc(driver ~ operator, data=d.pizza, rfrq=("000"), 
     verbose="high")                                  # no rel. frequencies 

Desc(price ~ delivery_min, data=d.pizza)              # numeric ~ numeric
Desc(price + delivery_min ~ operator + driver + wrongpizza,
     data=d.pizza, digits=c(2,2,2,2,0,3,0,0) ) 

Desc(week ~ driver, data=d.pizza, digits=c(2,2,2,2,0,3,0,0))   # define digits

Desc(delivery_min + weekday ~ driver, data=d.pizza) 


# without defining data-parameter
Desc( d.pizza$delivery_min ~ d.pizza$driver)           


# with functions and interactions
Desc( sqrt(price) ~ operator : factor(wrongpizza), data=d.pizza) 
Desc( log(price+1) ~ cut(delivery_min, breaks=seq(10,90,10))  
  , data=d.pizza, digits=c(2,2,2,2,0,3,0,0)) 


# internal functions (not meant to be used by the enduser):
Desc.factor(d.pizza$driver, ord="n")  # ordered by name
Desc.factor(d.pizza$driver, ord="l")  # ordered by level
Desc.logical(d.pizza$wrongpizza)
Desc.integer(d.pizza$weekday)
Desc.integer(d.pizza$weekday, maxlevels=3)
Desc.numeric(d.pizza$count, highlow=FALSE)
Desc.numeric(d.pizza$count, highlow=TRUE)
 
DescNumFact(x=d.pizza$delivery_min, grp=d.pizza$operator)
DescNumNum(x=d.pizza$delivery_min, y=d.pizza$price)
}
\keyword{ print }
