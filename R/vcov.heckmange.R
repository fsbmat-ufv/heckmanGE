#' vcov.heckmanGE
#' Variance-Covariance Matrix of the Generalized Heckman Model
#'
#' Extracts the variance-covariance matrix of the coefficients for the Generalized Heckman model.
#' The matrix can be for specific parts of the model or the complete matrix.
#'
#' @param object An object of class `heckmanGE` representing a fitted Generalized Heckman model.
#' @param part A character vector specifying the parts of the model to include in the variance-covariance matrix.
#'             Options are "selection", "outcome", "dispersion", and "correlation".
#'             By default, the function returns the complete variance-covariance matrix for all parts.
#' @param \dots Additional arguments passed to or from other methods. These are not used in this method but must be included to match the generic method signature.
#'
#' @return
#' A variance-covariance matrix of the coefficients for the specified parts of the Generalized Heckman model.
#' @export
vcov.heckmanGE = function(object, part = c("selection", "outcome", "dispersion", "correlation"), ...){

        indexes = NULL
        if("selection"   %in% part) indexes = c(indexes, object$coefficients_indexes$index.selection)
        if("outcome"     %in% part) indexes = c(indexes, object$coefficients_indexes$index.outcome)
        if("dispersion"  %in% part) indexes = c(indexes, object$coefficients_indexes$index.dispersion)
        if("correlation" %in% part) indexes = c(indexes, object$coefficients_indexes$index.correlation)

        object$vcov[indexes, indexes]
}

