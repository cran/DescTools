\name{SysInfo}
\alias{SysInfo}
\alias{DescToolsOptions}
\title{System Information And DescTools Options}
\description{SysInfo is a convenience function to compile some information about the
computing system and environment used.}
\usage{SysInfo()
DescToolsOptions()}
\details{The function SysInfo is mainly used to save the system environment information
in ncdf files containing the results of some calculations. \cr\cr
\bold{Options}\cr
There are a few options for the graphical output that can be set. DescToolsOptions displays the currently defined options.\cr
\bold{1) Footnotes}\cr
In some tables there are footnote signs used. They're named \code{footnote1}, \code{footnote2} and can be changed with e.g. \code{options("footnote1"="*")}. Any character can be defined here.\cr
\bold{2) plotit} \cr
The option \code{plotit} can be used to make the Desc-procedures produce plots by default. Set: \code{options(plotit=TRUE)}. Valid values are \code{TRUE} and \code{FALSE}.\cr
\bold{3) Colors}\cr
Three colors, that are used in many places can be set as options too. The options are \code{col1}, \code{col2} and \code{col3}. By default they're set to \code{hred}, \code{hblue} and \code{horange}. Change the values by defining \code{options(col1="pink", col2="blue", col2="yellow")}. Any color definition can be used here. 

}
\value{character string with all version and system information of the current R system}

\author{Jannis v. Buttlar <jbuttlar@bgc-jena.mpg.de>, Andri Signorell <andri@signorell.net>}





