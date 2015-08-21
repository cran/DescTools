\name{BinTree}
\alias{BinTree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Binary Tree
%%  ~~function to do ... ~~
}
\description{Create a binary tree
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BinTree(n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{integer, size of the tree
%%     ~~Describe \code{n} here~~
}
}
\details{
If we index the nodes of the tree as 1 for the top, 2--3 for the next 
horizontal row, 4--7 for the next, \ldots then the parent-child 
traversal becomes particularly easy.
The basic idea is that the rows of the tree start at indices 1, 2, 4, \ldots.

BinTree(13) yields the vector
[[8  4  9  2 10  5 11  1 12  6 13  3  7]] meaning that the smallest element
will be in position 8 of the tree, the next smallest in position 4, etc.

%%  ~~ If necessary, more details than the description above ~~
}
\value{a vector
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Terry Therneau <therneau.terry@mayo.edu>
%%  ~~who you are~~
}

\examples{
BinTree(12)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ list }
