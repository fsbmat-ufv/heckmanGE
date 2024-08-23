#' vcov.fastHeckmanGe
#'
#' Variance-covariance matrix of the Generalized Heckman Model
#'
#' @param object fastHeckmanGe class object.
#' @param part A character vector with the model part names: selection, outcome, dispersion, correlation. By default it returns the complete vcov matrix, for all parts.
#' @return
#' The variance-covariance matrix of the coefficients of Generalized Heckman model.
#' @export vcov.fastHeckmanGe
#' @export
vcov.fastHeckmanGe = function(object, part = c("selection", "outcome", "dispersion", "correlation")){

        indexes = NULL
        if("selection"   %in% part) indexes = c(indexes, object$coefficients_indexes$index.selection)
        if("outcome"     %in% part) indexes = c(indexes, object$coefficients_indexes$index.outcome)
        if("dispersion"  %in% part) indexes = c(indexes, object$coefficients_indexes$index.dispersion)
        if("correlation" %in% part) indexes = c(indexes, object$coefficients_indexes$index.correlation)

        object$vcov[indexes, indexes]
}

