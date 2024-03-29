% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opCharMethodFunctions.R
\name{IndProb}
\alias{IndProb}
\title{Extract the individual probabilities used to calculate group testing
results}
\usage{
IndProb(object, ...)
}
\arguments{
\item{object}{An object of class "opChar", from which the individual
probabilities are to be extracted.}

\item{...}{Additional arguments to be passed to \code{IndProb} (e.g.,
\code{digits} to be passed to \code{signif} for appropriate
rounding).}
}
\value{
Either \kbd{p.vec}, the sorted vector of individual probabilities
(for hierarchical group testing algorithms) or \kbd{p.mat}, the sorted
matrix of individual probabilities in gradient arrangement (for array
testing algorithms). Further details are given under the 'Details' section
for the \code{\link{operatingCharacteristics1}} (\kbd{opChar1})
or \code{\link{operatingCharacteristics2}} (\kbd{opChar2}) functions.
}
\description{
Extract the individual probabilities from objects of class
"opchar" returned by \code{\link{operatingCharacteristics1}} (\kbd{opChar1})
or \code{\link{operatingCharacteristics2}} (\kbd{opChar2}).
}
\examples{
config.mat <- matrix(data = c(rep(1, 10), 1:10),
                     nrow = 2, ncol = 10, byrow = TRUE)
res1 <- opChar1(algorithm = "D2", p = 0.05, Se = 0.99, Sp = 0.99,
        hier.config = config.mat)
IndProb(res1)

config.mat <- matrix(data = c(rep(1, 20), rep(1, 10), rep(2, 10),
                             rep(c(1, 2, 3, 4), each = 5),
                             rep(1, 3), rep(2, 2), rep(3, 3),
                             rep(4, 2), rep(5, 3), rep(6, 2),
                             rep(7, 3), rep(8, 2), 1:20),
                    nrow = 5, ncol = 20, byrow = TRUE)
Se <- matrix(data = rep(0.95, 10), nrow = 2, ncol = 5,
             dimnames = list(Infection = 1:2, Stage = 1:5))
Sp <- matrix(data = rep(0.99, 10), nrow = 2, ncol = 5,
             dimnames = list(Infection = 1:2, Stage = 1:5))
res2 <- opChar2(algorithm = "ID5",
                alpha = c(18.25, 0.75, 0.75, 0.25),
                Se = Se, Sp = Sp, hier.config = config.mat)
IndProb(res2)
}
\author{
Brianna D. Hitt
}
