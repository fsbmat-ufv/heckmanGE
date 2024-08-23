#' residuals.fastHeckmanGe
#'
#' Extract Residuals of the Generalized Heckman Model
#'
#' @param object fastHeckmanGe class object.
#' @param part A charecter vector with the model part names: either 'selection' or 'outcome' (default).
#' @return
#' Residuals extracted from the object object.
#' @export residuals.fastHeckmanGe
#' @export
residuals.fastHeckmanGe = function(object, part = c("selection", "outcome")){

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


