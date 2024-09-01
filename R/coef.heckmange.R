#' coef.heckmanGE
#'
#' Extract Coefficients from a Generalized Heckman Model
#'
#' This function extracts the coefficients from a `heckmanGE` class object. You can specify which parts of the model you want to retrieve the coefficients for: selection, outcome, dispersion, or correlation. By default, the function returns the complete coefficient vector for all parts.
#'
#' @param object An object of class `heckmanGE`, which contains the fitted model.
#' @param part A character vector indicating which parts of the model coefficients to return. Valid options are: "selection", "outcome", "dispersion", and "correlation". By default, all parts are included in the returned coefficients.
#' @param \dots Additional arguments passed to or from other methods. These are not used in this method but must be included to match the generic method signature.
#'
#' @return A numeric vector containing the coefficients extracted from the model object. The coefficients correspond to the specified model parts.
#'
#' @details
#' The `coef.heckmanGE` function retrieves coefficients from the `heckmanGE` model object based on the specified parts. The parts represent different components of the Heckman model:
#' - "selection": Coefficients related to the selection equation.
#' - "outcome": Coefficients related to the outcome equation.
#' - "dispersion": Coefficients related to the dispersion equation.
#' - "correlation": Coefficients related to the correlation between selection and outcome.
#'
#' The function collects the appropriate indices for the requested parts and returns the corresponding coefficients.
#'
#' @export
coef.heckmanGE = function(object,
                          part = c("selection",
                                           "outcome",
                                           "dispersion",
                                           "correlation"),
                          ...){

  indexes = NULL
  if("selection"   %in% part) indexes = c(indexes, object$coefficients_indexes$index.selection)
  if("outcome"     %in% part) indexes = c(indexes, object$coefficients_indexes$index.outcome)
  if("dispersion"  %in% part) indexes = c(indexes, object$coefficients_indexes$index.dispersion)
  if("correlation" %in% part) indexes = c(indexes, object$coefficients_indexes$index.correlation)
  object$coefficients[indexes]
}

coef.heckmanGE = function(object, part = c("selection", "outcome", "dispersion", "correlation"),...){

        indexes = NULL
        if("selection"   %in% part) indexes = c(indexes, object$coefficients_indexes$index.selection)
        if("outcome"     %in% part) indexes = c(indexes, object$coefficients_indexes$index.outcome)
        if("dispersion"  %in% part) indexes = c(indexes, object$coefficients_indexes$index.dispersion)
        if("correlation" %in% part) indexes = c(indexes, object$coefficients_indexes$index.correlation)
        object$coefficients[indexes]
}
