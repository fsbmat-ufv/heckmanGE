#' Extract Coefficients from a Generalized Heckman Model
#'
#' This function extracts the coefficients from a `heckmanGE` class object. You can specify which parts of the model you want to retrieve the coefficients for: selection, outcome, dispersion, or correlation. By default, the function returns the complete coefficient vector for all parts.
#'
#' @param object An object of class `heckmanGE`, which contains the fitted model.
#' @param part A character vector indicating which parts of the model coefficients to return. Valid options are: `"selection"`, `"outcome"`, `"dispersion"`, and `"correlation"`. Multiple parts can be specified. By default, all parts are included in the returned coefficients.
#' @param \dots Additional arguments passed to or from other methods. Currently, these are not used in this method but must be included to match the generic method signature.
#'
#' @return A numeric vector containing the coefficients extracted from the model object. The coefficients correspond to the specified model parts, returned in the order they are requested.
#'
#' @details
#' The `coef.heckmanGE` function retrieves coefficients from the `heckmanGE` model object based on the specified parts. The parts represent different components of the Heckman model:
#' - `"selection"`: Coefficients related to the selection equation.
#' - `"outcome"`: Coefficients related to the outcome equation.
#' - `"dispersion"`: Coefficients related to the dispersion equation.
#' - `"correlation"`: Coefficients related to the correlation between selection and outcome.
#'
#' By default, the function returns coefficients from all parts. You can specify one or more parts in the `part` argument to extract coefficients from specific components.
#'
#' @examples
#' data(MEPS2001)
#' selectEq  <- dambexp ~ age + female + educ + blhisp + totchr + ins + income
#' outcomeEq <- lnambx ~ age + female + educ + blhisp + totchr + ins
#' dispersion  <- ~ age + female + totchr + ins
#' correlation  <- ~ age
#' fit <- heckmanGE(selection = selectEq,
#'                  outcome = outcomeEq,
#'                  dispersion = dispersion,
#'                  correlation = correlation,
#'                  data = MEPS2001)
#' # Extracting all coefficients:
#' coef(fit)
#'
#' # Extracting only the selection and outcome coefficients:
#' coef(fit, part = c("selection", "outcome"))
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
