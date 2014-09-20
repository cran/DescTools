\name{UnitConv}
\alias{UnitConv}
\alias{d.units}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Unit Conversion
%%  ~~function to do ... ~~
}
\description{Simple function for common unit conversions.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
UnitConv(x, from_unit, to_unit)

data(d.units)

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
The following units can be choosen. If a conversion can't be found the function will return NA.

\tabular{ll}{
unit    \tab description  \cr
a    \tab year (assuming 365.25 days per year)  \cr
AA    \tab 1 angstrom = 100 pm  \cr
atm    \tab atmosphere (standard)  \cr
bar    \tab bar  \cr
cal    \tab calorie  \cr
C    \tab Celsius  \cr
d    \tab day  \cr
eV    \tab electronvolt  \cr
F    \tab Fahrenheit  \cr
ft    \tab foot = 12 inches  \cr
g    \tab gram, the SI unit of mass (although the definition is based on kg)  \cr
gal    \tab U.S. Gallon  \cr
h    \tab hour  \cr
hp    \tab horsepower (metric)  \cr
in    \tab inch (strangely, some people still use these)  \cr
J    \tab Joule, the Si unit of energy  \cr
l    \tab litre  \cr
lb    \tab international avoirdupois pound  \cr
m    \tab metre, the SI unit of length  \cr
m3    \tab cubic metre  \cr
mi    \tab mile = 5280 feet  \cr
min    \tab minute  \cr
oz    \tab ounce  \cr
Pa    \tab Pascal, the SI unit of pressure  \cr
psi    \tab pound-force per square inch  \cr
s    \tab second, the SI unit of time  \cr
tn    \tab metric ton  \cr
toe    \tab 1 toe = 1000 kgoe = 1000 kg * 35 MJ/kg = 35000 MJ (energy as tons of oil equivalent)  \cr
ton    \tab metric ton  \cr
W    \tab Watt, the SI unit of power  \cr
Wh    \tab 1 Wh = 1 Wh * 3600 s/h = 3600 Ws = 3600 MJ  \cr
yd    \tab yard (basic unit of length)  \cr
}

All the details can be found in the d.units data.frame.

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
