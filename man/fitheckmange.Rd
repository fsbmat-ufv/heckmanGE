% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitheckmanGE.R
\name{fitheckmanGE}
\alias{fitheckmanGE}
\title{fitheckmanGE}
\usage{
fitheckmanGE(start, YS, XS, YO, XO, Msigma, Mrho, w)
}
\arguments{
\item{start}{A numeric vector of initial parameter guesses for the selection,
outcome, dispersion, and correlation equations.}

\item{YS}{A binary vector indicating selection status (1 if selected, 0 otherwise).}

\item{XS}{A matrix of independent variables for the selection equation.}

\item{YO}{A numeric vector of observed outcomes (dependent variable) for the
outcome equation.}

\item{XO}{A matrix of independent variables for the outcome equation.}

\item{Msigma}{A matrix representing the predictors for the dispersion parameter.}

\item{Mrho}{A matrix representing the predictors for the correlation parameter.}

\item{w}{A numeric vector of observation weights, used in the likelihood
computation.}
}
\value{
A list with the following components:
\describe{
\item{coefficients}{Named vector of estimated coefficients for selection,
outcome, dispersion, and correlation equations.}
\item{fitted.values}{Named list with fitted values for each equation
(selection, outcome, dispersion, correlation).}
\item{residuals}{Numeric vector of residuals for the selection and outcome
equations.}
\item{loglik}{Log-likelihood value of the fitted model.}
\item{vcov}{Variance-covariance matrix of the estimated parameters.}
\item{aic}{Akaike Information Criterion (AIC) for the model.}
\item{bic}{Bayesian Information Criterion (BIC) for the model.}
\item{optimization}{Details of the optimization process, including
convergence information.}
}
}
\description{
Newton-Raphson Optimization for Generalized Heckman Model Estimation
}
\details{
This function estimates the parameters of a generalized Heckman selection
model using a Newton-Raphson optimization algorithm. It supports the modeling
of selection and outcome equations, along with associated dispersion and
correlation structures.

This function uses the Newton-Raphson algorithm to estimate the parameters of
a generalized Heckman model, which accounts for sample selection bias.
The model is composed of a selection equation (modeled by \code{YS} and \code{XS}), an
outcome equation (modeled by \code{YO} and \code{XO}), and additional equations for
dispersion (\code{Msigma}) and correlation (\code{Mrho}). The optimization process
maximizes the log-likelihood of the model, allowing for robust estimation of
selection bias, while also estimating associated dispersion and correlation
parameters.

The function outputs the coefficients, fitted values, residuals, and several
information criteria for model comparison.
}
