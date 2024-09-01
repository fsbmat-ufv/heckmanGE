#' model.frame.heckmanGE
#' Get the Model Frames of a Generalized Heckman Regression
#'
#' Extracts the model frames for different parts of a `heckmanGE` model. The model frames include the data used in the regression analysis for each component of the Generalized Heckman Model.
#'
#' @param formula An object of class `heckmanGE`. This object should be a fitted model from which the model frames will be extracted.
#' @param part A character vector specifying the model part for which to extract the model frame. Options include "selection", "outcome", "dispersion", and "correlation". The default is "outcome". If multiple parts are specified, only the "outcome" part will be returned.
#' @param \dots Additional arguments passed to or from other methods. These are not used in this method but must be included to match the generic method signature.
#'
#' @return
#' A model frame for the specified part of the `heckmanGE` object. If `part` is not one of the valid options, an error is raised.
#'
#' @details
#' - The function extracts the model frame corresponding to the specified part of the `heckmanGE` model.
#' - If the `part` argument is not specified correctly or includes multiple parts, the function defaults to returning the model frame for the "outcome" part.
#'
#' @export
model.frame.heckmanGE = function(formula,
                                 part = c("selection", "outcome", "dispersion", "correlation"),
                                 ...){

  if(!all(part %in% c("selection", "outcome", "dispersion", "correlation"))) {
    stop("part must be 'selection', 'outcome', 'dispersion', or 'correlation'")
  }

  if(length(part) > 1) {
    part = 'outcome'
  }

  formula$model.frames[[paste0("model.frame.",part)]]
}
