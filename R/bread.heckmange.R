#' bread.heckmange
#'
#' Bread Function for the `fitheckmange` Model
#'
#' This function calculates the "bread" component of the sandwich estimator for the `fitheckmange` model. The bread matrix is typically defined as the product of the number of observations and the variance-covariance matrix of the estimated parameters.
#'
#' @param object An object of class `fitheckmange`, which is the result of fitting a Heckman selection model using the `fitheckmange` function.
#' @param ... Additional arguments (currently unused).
#'
#' @return A matrix representing the bread component of the sandwich estimator. The matrix is calculated as the product of the number of observations and the variance-covariance matrix of the estimated parameters.
#'
#' @details
#' The bread matrix is an essential component of the sandwich estimator used to obtain robust standard errors. It reflects the variability in the estimated parameters due to the model's fit. The function uses the number of observations and the variance-covariance matrix from the `fitheckmange` model object to compute this matrix.
#'
#' @export
bread.heckmange <- function(object, ...) {

        object$nObs * object$vcov  # deveriamos usar nObs ou a soma dos pesos?
}
