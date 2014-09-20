\name{Recode}
\alias{Recode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Recode a Factor
%%  ~~function to do ... ~~
}
\description{Combining or rearranging a factor can be tedious if it has many levels. \code{Recode} supports this step by 
accepting a direct definition of newlevels by oldlevels as argument and adding an \code{"elselevel"} option.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Recode(x, newlevels, elselevel = NA, use.empty = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the factor whose levels are to be altered.
%%     ~~Describe \code{x} here~~
}
  \item{newlevels}{a list with the oldlevels combined by \code{c}() and named with the name of the new level:  
  \code{list(newlevel_a=c("old_a", "old_b"), }\code{newlevel_b=c("old_c", "old_d"))}. See examples.
%%     ~~Describe \code{newlevels} here~~
}
  \item{elselevel}{how should the levels, which are not matched by newlevel's list be named. 
  Set this to \code{NULL}, if elselevels should be left unchanged.
  Defaults to \code{NA}.
%%     ~~Describe \code{elselevel} here~~
}
  \item{use.empty}{logical. Defines how a new level, which can't be found in x, should be handled.  Should it be left in the level's list or be dropped? The default is FALSE, 
   which drops empty levels.
%%     ~~Describe \code{newlevels} here~~
}
}
\value{
the factor having the new levels applied.
}

\author{Andri Signorell <andri@signorell.net>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{factor}}, \code{\link{levels}}\cr
There's another possible solution in the package car. 
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- factor(sample(letters, 20))

y <- Recode( x, newlevels=list( 
  "good" = c("a","b","c"),           # the old levels "a","b","c" get the new level "good"
  "bad"  = c("d","e","f"),           # the old levels "d","e","f" get the new level "bad" etc.
  "ugly" = c("g","h","k")), elselevel="other")

data.frame(x, y)



x <- factor(letters[1:6])

z1 <- Recode(x, newlevels=list("AB"=c("a","b"), "CD"=c("c","d")), elselevel="none of these")
z2 <- Recode(x, newlevels=list("AB"=c("a","b"), "CD"=c("c","d")), elselevel=NA)
z3 <- Recode(x, newlevels=list("AB"=c("a","b"), "CD"=c("c","d")), elselevel=NULL)
z4 <- Recode(x, newlevels=list("AB"=c("a","b"), "GH"=c("g","h")), elselevel=NA, use.empty=TRUE)
z5 <- Recode(x, newlevels=list("AB"=c("a","b"), "GH"=c("g","h")), elselevel=NA, use.empty=FALSE)

data.frame(z1, z2, z3, z4, z5)

# empty level GH in z4...
table(z4, useNA="ifany")
# but not in z5
table(z5, useNA="ifany")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
