\name{UnitConv}
\alias{UnitConv}
\alias{d.units}
\alias{d.prefix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Unit Conversion
%%  ~~function to do ... ~~
}
\description{
Convert a number from one measurement system to another. The function can translate a table of distances in miles to a table of distances in kilometers.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
UnitConv(x, from_unit, to_unit)

data(d.units)
data(d.prefix)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the numeric to be converted.
%%     ~~Describe \code{x} here~~
}
  \item{from_unit}{a character defining the original unit. 
%%     ~~Describe \code{from_unit} here~~
}
  \item{to_unit}{a character defining the target unit.
%%     ~~Describe \code{to_unit} here~~
}
}
\details{
The following units can be chosen. Conversions will work with units within the group. \code{NA} wil be returned if a conversion can't be found. \cr
The multipliers can be found in the dataset \code{d.units}.

\tabular{lll}{
\cr\cr
\bold{Weight and mass}\tab \tab\cr
Gram  \tab g \tab metric\cr
Slug  \tab sg \cr
Pound mass (avoirdupois)  \tab lbm \cr
U (atomic mass unit)  \tab u \cr
Ounce mass (avoirdupois)  \tab ozm \cr
\cr\bold{Distance}\tab \cr
Meter  \tab  m \tab metric \cr
Statute mile  \tab mi \cr
Nautical mile  \tab Nmi \cr
Inch  \tab in \cr
Foot  \tab ft \cr
Yard  \tab yd \cr
Angstrom  \tab ang \tab metric \cr
Pica  \tab pica \cr
  \tab  \cr
\cr\bold{Time}\tab \cr
Year  \tab yr \cr
Day  \tab day \cr
Hour  \tab hr \cr
Minute  \tab mn \cr
Second  \tab sec \cr
\cr\bold{Pressure}\tab \cr
Pascal  \tab Pa (or p) \cr
Atmosphere  \tab atm (or at) \cr
mm of Mercury  \tab mmHg \cr
  \tab  \cr
\cr\bold{Force}\tab \cr
Newton  \tab N \tab metric \cr
Dyne  \tab dyn (or dy) \cr
Pound force  \tab lbf \cr
\cr\bold{Energy}\tab \cr
Joule  \tab J \tab metric \cr
Erg  \tab e \cr
Thermodynamic calorie  \tab c \cr
IT calorie  \tab cal \tab metric \cr
Electron volt  \tab eV (or ev) \tab metric \cr
Horsepower-hour  \tab HPh (or hh) \cr
Watt-hour  \tab Wh (or wh) \tab metric \cr
Foot-pound  \tab flb \cr
BTU  \tab BTU (or btu) \cr
  \tab  \cr
\cr\bold{Power}\tab \cr
Horsepower  \tab HP (or h) \cr
Watt  \tab W (or w) \tab metric \cr
\cr\bold{Magnetism}\tab \cr
Tesla  \tab T \tab metric \cr
Gauss  \tab ga \tab metric \cr
  \tab  \cr
\cr\bold{Temperature}\tab \cr
Degree Celsius  \tab C (or cel) \cr
Degree Fahrenheit  \tab F (or fah) \cr
Kelvin  \tab K (or kel) \tab metric \cr
\cr\bold{Liquid measure}\tab \cr
Teaspoon  \tab tsp \cr
Tablespoon  \tab tbs \cr
Fluid ounce  \tab oz \cr
Cup  \tab cup \cr
U.S. pint  \tab pt (or us_pt) \cr
U.K. pint  \tab uk_pt \cr
Quart  \tab qt \cr
Gallon  \tab gal \cr
Liter  \tab l (or lt) \tab metric \cr
}


All the details can be found in the \code{d.units} data.frame.

%%  ~~ If necessary, more details than the description above ~~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\examples{
UnitConv(c(1.2, 5.4, 6.7), "in", "m")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arith }
