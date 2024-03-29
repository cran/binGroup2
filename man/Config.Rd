% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AddGenericFunctions.R
\name{Config}
\alias{Config}
\title{Access the testing configurations returned from an object}
\usage{
Config(object, ...)
}
\arguments{
\item{object}{An object from which the testing configurations are to be extracted.}

\item{...}{Additional arguments to be passed to \code{Config}.}
}
\description{
\code{Config} is a generic function that extracts testing configurations from an object
}
\examples{
# Find the optimal testing configuration for
#   non-informative two-stage hierarchical testing.
res1 <- OTC1(algorithm = "D2", p = 0.01, Se = 0.99, Sp = 0.99,
             group.sz = 2:100, obj.fn = c("ET", "MAR", "GR1"),
             weights = matrix(data = c(1,1), nrow = 1, ncol = 2))
Config(res1)
}
\seealso{
\code{\link{Config.opChar}} and \code{\link{Config.OTC}}
}
\author{
Christopher R. Bilder
}
