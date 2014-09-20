\name{DescTools-package}
\alias{DescTools-package}
\alias{DescTools}
\alias{options}
\docType{package}
\title{ Tools for Efficient Descriptive Statistics
}
\description{ 
DescTools contains a bunch of basic statistic functions and convenience wrappers for efficiently describing data, creating specific plots and using MS-Office (Word, Excel or PowerPoint) for doing reports.\cr 
The package's intention is to assemble a toolbox with basic functions to facilitate the (notoriously time consuming) first descriptive tasks in data analysis, consisting of calculating descriptive statistics, drawing graphical summaries and reporting the results.
Many of the included functions can be found scattered in other packages and other sources written partly by Titans of R. The reason for collecting them here, 
was primarily to have them consolidated in ONE instead of dozens of packages (which themselves might depend on other packages which are not needed at all), and to provide a common and consistent interface as far as function 
and arguments naming, NA handling, recycling rules etc. are concerned. In this sense this is my "Best-of"-collection of R-functions, I've happened to need in recent years. 
\cr
Google styleguides were used  as naming rules (in absence of convincing alternatives). The 'CamelStyle' was consequently 
applied to functions borrowed from contributed R packages as well. 

Feedback, feature requests, bugreports and other suggestions are welcome!

}
\details{
\tabular{ll}{
Package: \tab DescTools\cr
Type: \tab Package\cr
Version: \tab 0.99.8\cr
Date: \tab 2014-09-20\cr
Depends: \tab tcltk \cr
Suggests: \tab RDCOMClient\cr
Imports: \tab boot\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
LazyData: \tab yes\cr
ByteCompile: \tab yes\cr
NeedsCompilation: \tab yes\cr

}


A grouped list of the functions:  

\tabular{ll}{
\cr\cr
\bold{Operators, calculus, transformations:}\tab\cr
\link{\%()\%}    \tab      Between operators: \%()\%, \%(]\%, \%[)\%, \%[]\% \cr
\link{\%)(\%}    \tab      and their negations: \%)(\%, \%](\%, \%)[\%, \%][\% \cr
\link{\%nin\%}    \tab     "not in" operator \cr
\link{\%overlaps\%}    \tab     Determines whether two collections have common elements \cr
\link{\%like\%}    \tab     Simple operator to search for a specified pattern \cr
\link{AUC}        \tab      Calculate area under the curve \cr
\link{Primes}     \tab      Find all primes less than n \cr
\link{Factorize}  \tab      Prime factorization of integers \cr
\link{GCD}      \tab    Calculate the greatest common divisor\cr
\link{LCM}      \tab    Calculate the least common multiple\cr
\link{Permn}      \tab      Determine all possible permutations of a set \cr
\link{Fibonacci}  \tab      Generates single Fibonacci numbers or a Fibonacci sequence\cr
\link{Frac}       \tab      Return the fractional part of a numeric value \cr
\link{Ndec}       \tab      Count decimal places of a number \cr
\link{BoxCox}, \link{BoxCoxInv}     \tab      Box Cox transformation and inverse transformation \cr
\link{LogGen}, \link{LogLin}     \tab      Log linear hybrid, generalized log \cr
\link{LogSt}, \link{LogStInv}      \tab      Calculate started logarithmic transformation and it's inverse\cr
\link{Logit}, \link{LogitInv}      \tab      Generalized logit and inverse logit function \cr
\link{Winsorize}  \tab      Data cleaning by winsorization \cr
\link{Trim}  \tab      Trim data by omitting outlying observations \cr
\link{Recode}      \tab            Recode a factor with altered levels  \cr
\link{Rename}      \tab            Change name(s) of a named object \cr
\link{Sort}      \tab            Sort extension for matrices and data.frames \cr
\link{Large}, \link{Small}  \tab    Returns the kth largest, resp. smallest values\cr
\link{Rev}      \tab            Reverses the order of rows and columns of a matrix \cr
\link{Untable}      \tab            Recreates original list based on a n-dimensional frequency table \cr
\link{Dummy}      \tab      Generate dummy codes for a factor \cr
\link{FisherZ}, \link{FisherZInv}      \tab      Fisher's z-transformation and its inverse \cr
\cr\cr
\bold{Information and manipulation functions:}\tab\cr
\link{GetAllSubsets}       \tab      Generates all possible subsets out of a list of elements \cr
\link{GetPairs}      \tab            Generates all pairs out of one or two sets of elements \cr
\link{WhichFlags}   \tab      Returns the names of dichotomous columns of a data.frame  \cr
\link{WhichFactors}      \tab            Returns the names of factors in a given data.frame  \cr
\link{WhichNumerics}      \tab            Returns the names of numerical columns of a data.frame \cr
\link{IsWhole}      \tab            Is x a whole number? \cr
\link{Label}      \tab            Get or set the label attribute of an object \cr
\link{Mbind}  \tab  Bind matrices to 3-dimensional arrays     \cr
\link{VecRot}  \tab  Shift the elements of a vector in a circular mode to the right \cr
\tab or to the left by n characters.     \cr
\link{LOCF}      \tab         Imputation of data following the "last observation \cr
\tab carried forward" rule \cr
\cr\cr
\bold{String functions:}\tab\cr
\link{StrTrim}       \tab      Delete white spaces from a string \cr
\link{StrTrunc}   \tab      Truncate string on a given length and add ellipses if it really \cr
\tab was truncated  \cr
\link{StrAbbr}       \tab      Abbreviates a string \cr
\link{StrCap}      \tab            Capitalize the first letter of a string  \cr
\link{StrDist}      \tab            Compute distances between strings \cr
\link{StrRev}      \tab            Reverse a string \cr
\link{StrCountW}      \tab            Count the words in a string \cr
\link{StrChop}      \tab          Split a string by a fixed number of characters. \cr
\link{StrVal}      \tab          Extract numeric values from a string \cr
\link{StrPos}       \tab      Find position of first occurrence of a string in another one \cr
\link{StrIsNumeric}       \tab    Check whether a string does only contain numeric data \cr
\cr\cr
\bold{Conversion functions:}\tab\cr 
\link{AscToChar}, \link{CharToAsc}       \tab      Converts ASCII codes to characters and vice versa  \cr
\link{DecToBin}, \link{BinToDec}   \tab      Converts numbers from binmode to decimal and vice versa  \cr 
\link{DecToHex}, \link{HexToDec}   \tab      Converts numbers from hexmode to decimal and vice versa  \cr 
\link{DecToOct}, \link{OctToDec}   \tab      Converts numbers from octmode to decimal and vice versa  \cr
\link{DegToRad}, \link{RadToDeg}      \tab            Convert Degrees to Radians and vice versa  \cr
\link{CartToPol}, \link{PolToCart}     \tab            Transform Cartesian to Polar Coordinates and vice versa \cr
\cr\cr
\bold{Colors:}\tab\cr
\link{SetAlpha}   \tab  Add an alpha channel to a color.     \cr
\link{ChooseColorDlg}  \tab   Display the system's color dialog to choose a color     \cr
\link{PlotRCol}    \tab  Display R colors in a dialog\cr
\link{ColorLegend}  \tab   Add a color legend to a plot    \cr
\link{ColToGray}, \link{ColToGrey}  \tab  Convert colors to gcrey/grayscale     \cr
\link{ColToHex}, \link{HexToCol}  \tab   Convert a color into hex string    \cr
\link{ColToHsv}  \tab   R color to HSV conversion  \cr
\link{ColToRgb}, \link{RgbToCol}  \tab    Color to RGB conversion and back   \cr
\link{FindColor}  \tab   Get color on a defined color range    \cr
\link{MixColor}  \tab   Get the mix of two colors  \cr
\link{TextContrastColor} \tab   Choose textcolor depending on background color    \cr
\link{PalRedToBlack}, \link{PalTibco}  \tab   Defined color palettes    \cr
\cr\cr
\bold{Plots:}\tab\cr
\link{Canvas}  \tab    Canvas for geometric plotting   \cr
\link{Mar}  \tab    Set margins more comfortably.  \cr
\link{lines.loess}   \tab    Add a loess smoother to an existing plot   \cr
\link{ErrBars}   \tab    Add horizontal or vertical error bars to an existing plot   \cr
\link{DrawArc}, \link{DrawRegPolygon}  \tab  Draw elliptic, circular arc(s) or regular polygon(s)     \cr
\link{DrawCircle}, \link{DrawEllipse}  \tab  Draw a circle, ellipse   \cr
\link{DrawBezier}  \tab    Draw a Bezier curve   \cr
\link{DrawAnnulus}, \link{DrawAnnulusSector}  \tab    Draw one or several annuli, resp. sector of an annulus  \cr
\link{DrawBand}    \tab  Draw confidence band  \cr
\link{BoxedText}  \tab   Add text surrounded by a box to a plot    \cr
\link{Rotate}  \tab  Rotate a geometric structure     \cr
\link{SpreadOut}  \tab  Spread out a vector of numbers so that there is a minimum \cr
\tab interval between any two elements. This can be used \cr
\tab to place textlabels in a plot so that they do not overlap.\cr
\link{AreaIdent}  \tab  Helps identifying all the points in a specific area.     \cr
\link{identify.formula}  \tab  Formula interface for \code{\link{identify}}.     \cr
\link{PtInPoly}  \tab  Identify all the points within a polygon. \cr
\link{PlotACF}, \link{PlotMonth}    \tab  Create a combined plot of a time series and its autocorrelation\cr
\tab and partial autocorrelation \cr
\link{PlotArea}    \tab  Create an area plot \cr
\link{PlotBag}    \tab  Create a two-dimensional boxplot \cr
\link{PlotBubble}    \tab  Draw a bubble plot \cr
\link{PlotCandlestick}    \tab  Plot candlestick chart \cr
\link{PlotCirc}    \tab  Create a circular plot \cr
\link{PlotCorr}    \tab Plot a correlation matrix  \cr
\link{PlotDesc}    \tab   Create a descriptive plot of a vector x dependent on its class  \cr
\link{PlotDotCI}    \tab  Plot a dotchart with confidence intervals \cr
\link{PlotDotCIp}    \tab   Plot a dotchart with binomial confidence intervals \cr
\link{PlotFaces}    \tab  Produce a plot of Chernoff faces  \cr
\link{PlotFdist}    \tab  Frequency distribution plot, combination of histogram, \cr
\tab boxplot and ecdf.plot \cr
\link{PlotMarDens}    \tab Scatterplot with marginal densities  \cr
\link{PlotMultiDens}    \tab  Plot multiple density curves \cr
\link{PlotPolar}    \tab Plot values on a circular grid \cr
\link{PlotFct}    \tab Plot mathematical expression or a function \cr
\link{PolarGrid}    \tab  Plot a grid in polar coordinates \cr
\link{PlotPyramid}    \tab  Pyramid plot (back-back histogram) \cr
\link{PlotTreemap}    \tab   Plot of a treemap.\cr
\link{PlotVenn}    \tab  Plot a Venn diagram \cr
\link{PlotViolin}    \tab   Plot violins instead of boxplots \cr
\link{PlotQQ}    \tab  QQ-plot for an optional distribution\cr
\link{PlotWeb}    \tab  Create a web plot \cr
\cr\cr
\bold{Statistics:}\tab\cr 
\link{Freq}  \tab    Frequency table   \cr
\link{PercTable}  \tab    Two dimensional percentage table   \cr
\link{Mode}  \tab     Mode, the most frequent value   \cr
\link{Gmean}, \link{Gsd}  \tab  Geometric mean and geometric standard deviation     \cr
\link{Hmean}  \tab    Harmonic Mean   \cr
\link{HuberM}, \link{TukeyBiweight}  \tab    Huber M-estimator of location and Tukey's biweight robust mean   \cr
\link{MeanAD}  \tab    Mean absolute deviation   \cr
\link{MeanSE}  \tab   Standard error of mean    \cr
\link{Skew}, \link{Kurt} \tab   Skewness and kurtosis    \cr
\link{YuleQ}, \link{YuleY}  \tab  Yule's Q and Yule's Y      \cr
\link{TschuprowT}  \tab  Tschuprow's T     \cr
\link{Phi}, \link{ContCoef}, \link{CramerV}  \tab   Phi, Pearson's Contingency Coefficient and Cramer's V    \cr
\link{CohenKappa}, \link{Kappam}  \tab Cohen's Kappa and weighted Kappa and Kappa for \cr
\tab more than 2 raters      \cr
\link{CronbachAlpha}  \tab  Cronbach's alpha     \cr
\link{ICC}  \tab  Intraclass correlations   \cr
\link{GoodmanKruskalTauA}  \tab  Goodman Kruskal's tau-a     \cr
\link{GoodmanKruskalGamma}  \tab  Goodman Kruskal's gamma     \cr
\link{KendallTauB}  \tab  Kendall's tau-b     \cr
\link{StuartTauC}  \tab  Stuart's tau-c     \cr
\link{SomersDelta}  \tab  Somers' delta     \cr
\link{Lambda}  \tab  Goodman Kruskal's lambda     \cr
\link{UncertCoef}  \tab  Uncertainty coefficient     \cr
\link{TheilU} \tab  Theil's U1 and U2 coefficient     \cr
\link{Entropy}, \link{MutInf}  \tab  Shannon's entropy, mutual information     \cr
\link{OddsRatio}, \link{RelRisk}  \tab   Odds ratio and relative risk    \cr  
\link{Lc}  \tab   Calculate and plot Lorenz curve    \cr
\link{Gini}, \link{Atkinson}  \tab   Gini- and Atkinson coefficient    \cr
\link{Herfindahl}, \link{Rosenbluth}  \tab  Herfindahl- and Rosenbluth coefficient      \cr
\link{BinomCI}, \link{MultinomCI}  \tab   Confidence intervals for binomial and multinomial proportions    \cr
\link{BinomDiffCI}  \tab   Calculate confidence interval for a risk difference  \cr
\link{MeanCI}, \link{MedianCI}  \tab    Confidence interval for the mean and median   \cr
\link{MeanDiffCI}  \tab    Confidence interval for the difference of two means \cr
\link{VarCI}  \tab    Confidence interval for the variance   \cr
\link{CorCI}  \tab    Confidence interval for Pearson's correlation coefficient \cr
\link{RobScale}  \tab   Robust data standardization  \cr
\link{RobRange}  \tab   Robust range  \cr
\link{Outlier}  \tab   Outliers following Tukey's boxplot definition  \cr
\link{LOF}  \tab  Local Outlier Factor  \cr
\cr\cr
\bold{Tests:}\tab\cr 
\link{SignTest}    \tab  Signtest\cr
\link{ZTest}    \tab   Z--test for known population variance \cr
\link{JonckheereTerpstraTest}    \tab  Jonckheere-Terpstra test\cr
\link{PageTest}    \tab  Page test for ordered alternatives\cr
\link{CochranQTest}    \tab  Cochran's Q-test\cr
\link{SiegelTukeyTest}    \tab  Siegel-Tukey test for equality in variability \cr
\link{LeveneTest}    \tab  Levene's test for homogeneity of variance\cr
\link{MosesTest}    \tab  Moses Test of extreme reactions\cr
\link{RunsTest}    \tab  Runs test for randomness\cr
\link{BartelsRankTest}    \tab  Bartels rank test for randomness\cr
\link{JarqueBeraTest}    \tab  Jarque--Bera Test\cr
\link{AndersonDarlingTest}    \tab  Anderson-Darling test for normality\cr
\link{CramerVonMisesTest}    \tab  Cramer-von Mises test for normality\cr
\link{LillieTest}    \tab  Lilliefors (Kolmogorov-Smirnov) test for normality\cr
\link{PearsonTest}    \tab  Pearson chi-square test for normality\cr
\link{ShapiroFranciaTest}    \tab  Shapiro-Francia test for normality\cr
\link{MHChisqTest}    \tab  Mantel-Haenszel Chisquare test\cr
\link{StuartMaxwellTest}    \tab  Stuart-Maxwell marginal homogeneity test\cr
\link{LehmacherTest}    \tab  Lehmacher marginal homogeneity test\cr
\link{CochranArmitageTest}    \tab  Cochran-Armitage test for trend in binomial proportions\cr
\link{BreslowDayTest}, \link{WoolfTest}   \tab  Test for homogeneity on 2x2xk tables over strata\cr
\link{PostHocTest}    \tab  Post hoc tests by Scheffe, LSD, Tukey for a aov-object\cr
\link{ScheffeTest}   \tab  Multiple comparisons Scheffe test\cr
\link{HotellingsT2Test}   \tab  Hotelling's T2 test for the one and two sample case.\cr
\cr\cr
\bold{Date functions:}\tab\cr
\link{AddMonths}  \tab  Add a number of months to a given date     \cr
\link{IsDate} \tab  Check whether x is a date object     \cr
\link{IsWeekend} \tab  Check whether x falls on a weekend     \cr
\link{IsLeapYear} \tab  Check whether x is a leap year     \cr
\link{Date}  \tab     Create a date from numeric representation of year, month, day   \cr
\link{Day}, \link{Month}, \link{Year}  \tab     Extract part of a date  \cr
\link{Hour}, \link{Min}, \link{Sec}  \tab     Extract part of time   \cr
\link{Week}, \link{Weekday}  \tab    Returns ISO week and weekday of a date   \cr
\link{Quarter}  \tab  Quarter of a date     \cr
\link{YearDay}, \link{YearMonth}  \tab  The day in the year of a date     \cr
\link{Now}, \link{Today}  \tab     Get current date or date-time  \cr
\link{HmsToSec}, \link{SecToHms}  \tab   Convert h:m:s times to seconds and vice versa  \cr
\link{Zodiac}  \tab  The zodiac sign of a date :-)   \cr
\cr\cr
\bold{Finance functions:}\tab\cr
\link{OPR}  \tab  One period returns (simple and log returns)\cr
\link{NPV}  \tab  Net present value\cr
\link{IRR} \tab  Internal rate of return\cr
\cr\cr
\bold{GUI-Helpers:}\tab\cr
\link{ChooseColorDlg}    \tab  Display color dialog to choose a color \cr
%% \link{DescDlg}    \tab  Display a Dialog Containing Univariate Descriptive Statistis and a Plot \cr
\link{ImportDlg}    \tab  Get path of a data file to be opened \cr
\link{SelectVarDlg}    \tab  Select elements of a set by click \cr
\link{PasswordDlg}    \tab  Display a dialog containing an edit field, showing only ***. \cr
\link{PlotPar}   \tab   Display the R plot parameters in a dialog    \cr
\link{Explore}   \tab   A breeze of interactive plotting \cr
\cr\cr
\bold{Reporting:}\tab\cr
\link{CatTable}  \tab  Print a table with the option to have controlled linebreaks \cr
\link{FormatFix}  \tab  Format a figure with a fixed number of digits      \cr
\link{Desc}  \tab   Produce a rich description of an object    \cr
\link{DescWrd}  \tab       Produce the same description as above but send the results to \cr
\tab a Word document and add an adequate graphic representation \cr
\link{GetCurrWrd}    \tab  Get a handle to a running Word instance \cr
\link{GetNewWrd}    \tab   Create a new Word Instance  \cr
\link{WrdCaption}    \tab Insert a title in Word   \cr
\link{WrdPlot}    \tab   Insert the active plot to Word \cr
\link{WrdR}    \tab  Insert an R command and its output in a Word document \cr
\link{WrdSetFont}    \tab  Set the font in Word \cr
\link{WrdTable}    \tab  Insert a table in Word  \cr
\link{WrdText}    \tab  Insert normal text to Word \cr
\link{WrdInsertBookmark}    \tab Insert a new bookmark in a Word document \cr
\link{WrdGoto}    \tab Place cursor to a specific bookmark, or another text position. \cr
\link{WrdUpdateBookmark}    \tab Update the text of a bookmark's range\cr
\link{GetNewXL}    \tab  Create a new Excel instance \cr
\link{XLGetRange}    \tab  Get the values of one or several cell range(s) in Excel \cr
\link{XLGetWorkbook}    \tab  Get the values of all sheets of an Excel workbook \cr
\link{XLView}    \tab   Use Excel as viewer for a data.frame \cr
\link{GetNewPP}    \tab  Create a new PowerPoint instance \cr
\link{PpPlot}    \tab   Insert active plot to PowerPoint \cr
\cr\cr
\bold{Tools:}\tab\cr
\link{LsFct}, \link{LsData}, \link{LsObj}      \tab  List the functions (or the data, all objects) of a package \cr
\link{FctArgs}      \tab            Retrieve the arguments of a functions \cr
\link{ParseFormula}      \tab     Parse a formula and return the splitted parts of if \cr
\link{Keywords}    \tab  Get the keywords of a man page\cr
\link{SysInfo}    \tab  Get some more information about system and environment\cr
\link{DescToolsOptions}    \tab  Get the DescTools specific options\cr
}
}

\section{MS-Office}{To make use of MS-Office features you must have Office in one of its variants installed. 
All Wrd*, XL* and Pp* functions require as well the package RDCOMClient to be installed. Hence the use of these functions is restricted to Windows systems.
RDCOMClient is available at: \cr\url{http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/3.0/}.
RDCOMClient does not exist for Mac or Linux, sorry.
}

\section{Warning}{This package is still under development. Although the code seems meanwhile quite stable, until release of version 1.0 (which is expected by the dawn of 2015) you should be aware that everything in the package might be subject to change. Backward compatibility is not yet guaranteed. Functions may be deleted or renamed and new syntax may be inconsistent with earlier versions. By release of version 1.0 the "deprecated-defunct process" will be installed.}


\author{
Andri Signorell\cr 
Helsana Versicherungen AG, Health Sciences, Zurich\cr
HWZ University of Applied Sciences in Business Administration Zurich.\cr
 
Includes R source code and/or documentation previously published by (in alphabetical order): \cr
Nanina Anderegg, Tomas Aragon, Antti Arppe, Adrian Baddeley, Ben Bolker, Frederico Caeiro, Stephane Champely, Daniel Chessel, Leanne Chhay, Michael Dewey, Harold C. Doran, Stephane Dray, Charles Dupont, Jeff Enos, Claus Ekstrom, Martin Elff, John Fox, Tal Galili, Matthias Gamer, Joseph L. Gastwirth, Yulia R. Gel, Juergen Gross, Gabor Grothendieck, Frank E. Harrell Jr, Michael Hoehle, Christian W. Hoffmann, Markus Huerzeler, Wallace W. Hui, Rob J. Hyndman, Pablo J. Villacorta Iglesias, Matthias Kohl, Mikko Korpela, Detlew Labes, Friederich Leisch, Jim Lemon, Martin Maechler, Arni Magnusson, Daniel Malter, George Marsaglia, John Marsaglia, Alina Matei, David Meyer, Weiwen Miao, Yongyi Min, Markus Naepflin, Daniel Navarro, Klaus Nordhausen, Derek Ogle, Nick Parsons, Sandrine Pavoine, Roland Rapold, William Revelle, Tyler Rinker, Caroline Rodriguez, Venkatraman E. Seshan, Greg Snow, Michael Smithson, Werner A. Stahel, Yves Tille, Adrian Trapletti, Kevin Ushey, Jeremy VanDerWal, Bill Venables, John Verzani, Gregory R. Warnes, Stefan Wellek, Peter Wolf, Daniel Wollschlaeger, Achim Zeileis

Thank you all!

Special thanks go to Daniel Wollschlaeger for his valuable contributions.


Maintainer: Andri Signorell <andri@signorell.net>

}


\keyword{ package }


