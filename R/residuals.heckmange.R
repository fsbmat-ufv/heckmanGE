#' residuals.heckmange
#' Extract Residuals of the Generalized Heckman Model
#'
#' Extracts residuals from a fitted `heckmange` model for a specified model component.
#'
#' @param object An object of class `heckmange`. This object should be a fitted model from which residuals are to be extracted.
#' @param part A character vector specifying which model component's residuals to return: either 'selection' or 'outcome'. Defaults to 'outcome'.
#' @param \dots Additional arguments passed to or from other methods. These are not used in this method but must be included to match the generic method signature.
#'
#' @return
#' A vector of residuals extracted from the specified part of the `heckmange` model.
#' @importFrom stats residuals
#' @export
residuals.heckmange = function(object, part = c("selection", "outcome"), ...){

        if(!all(part %in% c("selection", "outcome"))) {
                stop("part must be 'selection' or 'outcome'")
        }

        if(length(part) > 1) {
                part = 'outcome'
        }

        if(part == "outcome"){
                residuals = object$residuals$residuals.outcome
        }else{
                residuals = object$residuals$residuals.selection
        }

        return(residuals)
}


