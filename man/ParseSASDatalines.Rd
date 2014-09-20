\name{ParseSASDatalines}
\alias{ParseSASDatalines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Parse a SAS Dataline Command
%%  ~~function to do ... ~~
}
\description{A parser for simple SAS dataline command texts. A data.frame is being built with the columnnames listed in the input section. Further format codes or similar are not supported.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ParseSASDatalines(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the SAS text 
%%     ~~Describe \code{x} here~~
}
}
\details{The function is designed for quickly import SAS data. More complex command structures in the INPUT-section are not supported.
%%  ~~ If necessary, more details than the description above ~~
}
\value{a data.frame
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{scan}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
txt <- " 
DATA survey; 
INPUT id sex $ age inc r1 r2 r3 ; 
DATALINES; 
1   F  35 17  7 2 2 
17  M  50 14  5 5 3 
33  F  45  6  7 2 7 
49  M  24 14  7 5 7 
65  F  52  9  4 7 7 
81  M  44 11  7 7 7 
2   F  34 17  6 5 3 
18  M  40 14  7 5 2 
34  F  47  6  6 5 6 
50  M  35 17  5 7 5 
; 
" 

(d.frm <- ParseSASDatalines(txt)) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }
