#' model.matrix.heckmange
#' Get the Design Matrices of a Generalized Heckman Regression
#'
#' Extracts the design matrices for different parts of a `heckmange` model. The design matrices include the predictors used in the regression analysis for each component of the Generalized Heckman Model.
#'
#' @param object An object of class `heckmange`. This object should be a fitted model from which the design matrices will be extracted.
#' @param part A character vector specifying the model part for which to extract the design matrix. Options include "selection", "outcome", "dispersion", and "correlation". The default is "outcome". If multiple parts are specified, only the "outcome" part will be returned.
#' @param \dots Additional arguments passed to or from other methods. These are not used in this method but must be included to match the generic method signature.
#'
#' @return
#' A design matrix for the specified part of the `heckmange` object. If `part` is not one of the valid options, an error is raised.
#'
#' @details
#' - The function extracts the design matrix corresponding to the specified part of the `heckmange` model.
#' - If the `part` argument is not specified correctly or includes multiple parts, the function defaults to returning the design matrix for the "outcome" part.
#'
#' @export
model.matrix.heckmange = function(object,
                                 part = c("selection", "outcome", "dispersion", "correlation"), ...){

        if(!all(part %in% c("selection", "outcome", "dispersion", "correlation"))) {
                stop("part must be 'selection', 'outcome', 'dispersion', or 'correlation'")
        }

        if(length(part) > 1) {
                part = 'outcome'
        }

        object$model.matrices[[paste0("X.",part)]]
}
