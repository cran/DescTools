\name{Fibonacci}
\alias{Fibonacci}
\title{
  Fibonacci Series
}
\description{
  Generates single Fibonacci numbers or a Fibonacci sequence.
}
\usage{
Fibonacci(n, sequence = TRUE)
}
\arguments{
  \item{n}{integer.}
  \item{sequence}{logical; default is TRUE.}
}
\details{
  Generates the \code{n}-th Fibonacci number, or the whole Fibonacci sequence
  from the first to the \code{n}-th number. 
  
  The recursive version (\code{sequence=FALSE}) is extremely slow for values
  \code{n>=30}. To get the \code{n}-th Fibonacci number for larger \code{n}
  values, use \code{Fibonacci(n)[n]}.
}
\value{
  A single integer, or a vector of integers.
}
\note{This function stems from the library \pkg{numbers} (\code{fibonacci}).}

\author{Hans W Borchers <hwborchers at googlemail.com>}

\examples{
Fibonacci(0)                            # 1
Fibonacci(2)                            # 2
Fibonacci(2, sequence = TRUE)           # 1 2


# Golden ratio
F <- Fibonacci(25, sequence = TRUE)     # ... 75025 121393
f25 <- F[25]/F[24]                      #     1.618033989
phi <- (sqrt(5) + 1)/2
abs(f25 - phi)                          # 7.945178e-11

# Fibonacci numbers w/o iteration
  fibo <- function(n) {
    phi <- (sqrt(5) + 1)/2
    fib <- (phi^(n+1) - (1-phi)^(n+1)) / (2*phi - 1)
    round(fib)
  }
fibo(30:33)                             # 1346269 2178309 3524578 5702887

# Compare recursive with iterative approach:
# system.time(F30 <- Fibonacci(30, sequence = FALSE)      # user: 17.075 s
# system.time(F30 <- Fibonacci(30, sequence = TRUE)[30])  # user:  0.006 s
}
