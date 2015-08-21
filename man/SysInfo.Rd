\name{SysInfo}
\alias{SysInfo}
\alias{DescToolsOptions}
\title{System Information And DescTools Options}
\description{SysInfo is a convenience function to compile some information about the
computing system and environment used.}
\usage{SysInfo()
DescToolsOptions(default = FALSE)}

\arguments{
  \item{default}{logical, if set to TRUE the DescTools options will be reset to their default values.
}
}

\details{The function SysInfo is mainly used to save the system environment information
in ncdf files containing the results of some calculations. \cr\cr
\bold{Options}\cr
There are a few options for the graphical output that can be set. DescToolsOptions displays the currently defined options.\cr
\bold{1) Footnotes}\cr
In some tables there are footnote signs used. They're named \code{footnote1}, \code{footnote2} and can be changed with e.g. \code{options("footnote1"="*")}. Any character can be defined here.\cr
\bold{2) plotit} \cr
The option \code{plotit} can be used to make the Desc-procedures produce plots by default. \cr Set: \code{options(plotit=TRUE)}. Valid values are \code{TRUE} and \code{FALSE}.\cr
\bold{3) Colors}\cr
Three colors, that are used in many places can be set as options too. The options are \code{col1}, \code{col2} and \code{col3}. By default they're set to \code{hred}, \code{hblue} and \code{horange}. Change the values by defining \code{options(col1="pink", col2="blue", col2="yellow")}. Any color definition can be used here. \cr
\bold{4) Format}\cr
Two format definitions are currently used in the Desc routines. For integer values there's a format named "fmt.abs" and for percentages one named "fmt.perc". The format definitions must be of class "fmt" and may contain any argument used in the function \code{\link{Format}}.\cr
\code{options(fmt.per=structure(list(digits=3, leading="drop"), class="fmt"))} would display percentages as .892\cr
and \code{options(fmt.per=structure(list(digits=1, fmt="\%"), class="fmt"))} as 89.2\%\cr
\code{options(fmt.abs=structure(list(digits=0, big.mark="'"), class="fmt"))} will display integers as 45'123\cr
}
\value{character string with all version and system information of the current R system}

\author{Jannis v. Buttlar <jbuttlar@bgc-jena.mpg.de>, Andri Signorell <andri@signorell.net>}





