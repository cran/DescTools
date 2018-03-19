\name{SelectVarDlg}
\alias{SelectVarDlg}
\alias{SelectVarDlg.default}
\alias{SelectVarDlg.factor}
\alias{SelectVarDlg.data.frame}
\alias{ModelDlg}

\title{Select Elements of a Set by Click}

\description{\code{SelectVarDlg()} is a GUI utility, which brings up a dialog and lets the user select elements (either variables of a data.frame or
levels of a factor) by point and click in a listbox. The list of selected items is written to the clipboard so
that the code can afterwards easily be pasted in the source file.\cr
\code{ModelDlg()} helps to compose model formulas based on the variablenames of a \code{data.frame}.
Both functions are best used together with the package \code{DescToolsAddIns}, with which the functions can be assigned to a keystroke in RStudio.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SelectVarDlg(x, ...)

\method{SelectVarDlg}{default}(x, useIndex = FALSE, ...)

\method{SelectVarDlg}{factor}(x, ...)

\method{SelectVarDlg}{data.frame}(x, ...)

ModelDlg(x, ...)


}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the object containing the elements to be selected. x can be a data.frame, a factor or any other vector.
%%     ~~Describe \code{x} here~~
}
  \item{useIndex}{defines, if the enquoted variablenames (default) or the index values should be returned.
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{further arguments to be passed to the default function. }

}

\details{
When working with big data.frames with many variables it is often tedious to build subsets by typing the
columnnames. Here is where the function comes in offering a "point and click" approach for selecting the interesting
columns.
When x is a \code{data.frame} the columnnames are listed,
when x is a factor the according levels are listed and in all other cases the list is filled with the unique elements of x.
%%  ~~ If necessary, more details than the description above ~~

In the model dialog, the variablenames of the selected data.frame are listed on the right, from where they can be inserted in the model box by clicking on a button between the two boxes. Clicking on the \code{+} button will use + to concatenate the variablenames.

\figure{ModelDlg.png}{Model dialog}

After clicking on ok, the formula \code{temperature ~ area + driver + delivery_min, data=d.pizza} will be inserted on the cursor position.
}
\value{
A comma separated list with the selected values enquoted is returned invisibly as well as
written to clipboard for easy inserting the text in an editor afterwards.
}
\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{select.list}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
SelectVarDlg(x = d.pizza$driver)
SelectVarDlg(x = d.pizza, useIndex=TRUE)

SelectVarDlg(d.pizza$driver)

x <- replicate(10, paste(sample(LETTERS, 5, replace = TRUE), collapse=""))
SelectVarDlg(x)

ModelDlg(d.pizza)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
