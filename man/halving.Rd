% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/halving.R
\name{halving}
\alias{halving}
\title{Probability mass function for halving}
\usage{
halving(p, Se = 1, Sp = 1, stages = 2, order.p = TRUE)
}
\arguments{
\item{p}{a vector of individual risk probabilities.}

\item{Se}{sensitivity of the diagnostic test.}

\item{Sp}{specificity of the diagnostic test.}

\item{stages}{the number of stages for the halving algorithm.}

\item{order.p}{logical; if TRUE, the vector of individual risk probabilities
will be sorted.}
}
\value{
A list containing:
\item{pmf}{the probability mass function for the halving algorithm.}
\item{et}{the expected testing expenditure for the halving algorithm.}
\item{vt}{the variance of the testing expenditure for the halving
          algorithm.}
\item{p}{a vector containing the probabilities of positivity for each individual.}
}
\description{
Calculate the probability mass function for the number of tests
from using the halving algorithm.
}
\details{
Halving algorithms involve successively splitting a positive
testing group into two equal-sized halves (or as close to equal as possible)
until all individuals have been identified as positive or negative.
\eqn{S}-stage halving begins by testing the whole group of \eqn{I}
individuals. Positive groups are split in half until the final stage of the
algorithm, which consists of individual testing. For example, consider an
initial group of size \eqn{I=16} individuals. Three-stage halving (3H)
begins by testing the whole group of 16 individuals. If this group tests
positive, the second stage involves splitting into two groups of size 8.
If either of these groups test positive, a third stage involves testing each
individual rather than halving again. Four-stage halving (4H) would continue
with halving into groups of size 4 before individual testing. Five-stage
halving (5H) would continue with halving into groups of size 2 before
individual testing. 3H requires more than 2 individuals, 4H requires more
than 4 individuals, and 5H requires more than 8 individuals.

This function calculates the probability mass function, expected testing
expenditure, and variance of the testing expenditure for halving algorithms
with 3 to 5 stages.
}
\examples{
# Equivalent to Dorfman testing (two-stage hierarchical)
halving(p = rep(0.01, 10), Se = 1, Sp = 1, stages = 2,
        order.p = TRUE)

# Halving over three stages; each individual has a
#   different probability of being positive
set.seed(12895)
p.vec <- expectOrderBeta(p = 0.05, alpha = 2, size = 20)
halving(p = p.vec, Se = 0.95, Sp = 0.95, stages = 3,
        order.p = TRUE)
}
\references{
\insertRef{Black2012}{binGroup2}
}
\seealso{
\code{\link{expectOrderBeta}} for generating a vector of individual risk
probabilities for informative group testing.

Other operating characteristic functions: 
\code{\link{GroupMembershipMatrix}()},
\code{\link{Sterrett}()},
\code{\link{TOD}()},
\code{\link{operatingCharacteristics1}()},
\code{\link{operatingCharacteristics2}()}
}
\author{
This function was originally written by Michael Black for Black
et al. (2012). The function was obtained from
\url{http://chrisbilder.com/grouptesting/}. Minor modifications have been
made for inclusion of the function in the \code{binGroup2} package.
}
\concept{operating characteristic functions}
