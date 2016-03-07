\name{Datasets for Simulation}
\alias{cards}
\alias{tarot}
\alias{roulette}
\docType{data}

\title{Datasets for Simulation
}

\description{For performing elementary probability calculations in introductory courses there are some datasets available.
The dataset roulette contains the standard sample space for one spin on a roulette wheel.
cards contain the standard set of 52 playing cards in four colours. tarot does the same with a classic tarot deck.
}

\usage{
cards
tarot
roulette
}
\value{
\code{cards} is a data.frame with two columns named \code{rank} and \code{suit}

\code{tarot} is a data.frame with three columns named \code{rank}, \code{suit} and \code{desc}

\code{roulette} is a data.frame with two columns named \code{num} and \code{col}
}

\examples{
head(cards)
head(tarot)
head(roulette)

# drawing 5 cards
sample(do.call(paste, cards), 5)

# spin the wheel
sample(do.call(paste, roulette), 1)

# simulate the evening in Las Vegas
sample(do.call(paste, roulette), 32, replace=TRUE)
}

\keyword{datasets}
