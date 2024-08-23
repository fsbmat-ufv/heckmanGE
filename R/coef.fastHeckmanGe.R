#' coef.fastHeckmanGe
#'
#' Extract Generalized Heckman Model Coefficients
#'
#' @param object fastHeckmanGe class object.
#' @param part A charecter vector with the model part names: selection, outcome, dispersion, correlation. By default it returns the complete coefficient vector, for all parts.
#' @return
#' Coefficients extracted from the model object.
#' @export
coef.fastHeckmanGe = function(object, part = c("selection", "outcome", "dispersion", "correlation")){

        indexes = NULL
        if("selection"   %in% part) indexes = c(indexes, object$coefficients_indexes$index.selection)
        if("outcome"     %in% part) indexes = c(indexes, object$coefficients_indexes$index.outcome)
        if("dispersion"  %in% part) indexes = c(indexes, object$coefficients_indexes$index.dispersion)
        if("correlation" %in% part) indexes = c(indexes, object$coefficients_indexes$index.correlation)

        object$coefficients[indexes]
}

