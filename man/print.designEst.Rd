% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/designFunctions.R
\name{print.designEst}
\alias{print.designEst}
\title{Print method for objects of class "designEst"}
\usage{
\method{print}{designEst}(x, ...)
}
\arguments{
\item{x}{an object of class "designEst" created by \code{\link{designEst}}.}

\item{...}{additional arguments to be passed to \code{print}.
Currently only \code{digits} to be passed to \code{signif} for
appropriate rounding.}
}
\value{
A print out detailing whether the bias restriction was violated,
whether the maximum allowed group size was reached, and the minimum MSE and
associated group size, expected value, variance, and bias.
}
\description{
Print method for objects of class "designEst" created by
\code{\link{designEst}}.
}
\author{
Brianna D. Hitt
}
