% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{simulation}
\alias{simulation}
\title{Simulation dataset for the heckmanGE example}
\format{
A data frame with 10,000 observations on the following variables:
\itemize{
\item{y_o: Outcome variable from the simulated model (numeric)}
\item{y_s: Selection indicator, 1 if selected, 0 otherwise (binary)}
\item{prob_s: Probability of selection (numeric)}
\item{x1: Simulated predictor from a normal distribution (numeric)}
\item{x2: Simulated predictor from a Poisson distribution (numeric)}
\item{x3: Simulated binary predictor (binary)}
\item{x4: Simulated predictor from a normal distribution with mean 2 and sd 2 (numeric)}
\item{x5: Simulated predictor from a Poisson distribution with lambda 1.5 (numeric)}
}
}
\usage{
simulation
}
\description{
This dataset contains simulated data used to illustrate the functionality
of the heckmanGE model. The data includes variables used in selection, outcome,
dispersion, and correlation equations.
}
\examples{
data(simulation)
selectEq  <- y_s ~ x1 + x2 + x4
outcomeEq <- y_o ~ x1 + x2 + x3
outcomeD  <- ~ x1 + x5
outcomeC  <- ~ x3 + x4
fit_heckmanGE <- heckmanGE(selection   = selectEq,
                           outcome     = outcomeEq,
                           dispersion  = outcomeD,
                           correlation = outcomeC,
                           data = simulation)
summary(fit_heckmanGE)
}
\keyword{datasets}
