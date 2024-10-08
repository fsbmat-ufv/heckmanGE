% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vcov.heckmanGE.R
\name{vcov.heckmanGE}
\alias{vcov.heckmanGE}
\title{vcov.heckmanGE
Variance-Covariance Matrix of the Generalized Heckman Model}
\usage{
\method{vcov}{heckmanGE}(
  object,
  part = c("selection", "outcome", "dispersion", "correlation"),
  ...
)
}
\arguments{
\item{object}{An object of class \code{heckmanGE} representing a fitted Generalized Heckman model.}

\item{part}{A character vector specifying the parts of the model to include in the variance-covariance matrix.
Options are "selection", "outcome", "dispersion", and "correlation".
By default, the function returns the complete variance-covariance matrix for all parts.}

\item{\dots}{Additional arguments passed to or from other methods. These are not used in this method but must be included to match the generic method signature.}
}
\value{
A variance-covariance matrix of the coefficients for the specified parts of the Generalized Heckman model.
}
\description{
Extracts the variance-covariance matrix of the coefficients for the Generalized Heckman model.
The matrix can be for specific parts of the model or the complete matrix.
}
