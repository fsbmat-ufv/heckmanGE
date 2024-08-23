#' fitted.fastHeckmanGe
#'
#' Extract Fitted Values of the Generalized Heckman Model
#'
#' @param object fastHeckmanGe class object.
#' @param part A character vector with the model part names: selection, outcome (default), dispersion, correlation.
#' @return
#' Fitted values extracted from the fastHeckmanGe object.
#' @export
fitted.fastHeckmanGe = function(object,
                                part = c("selection", "outcome", "dispersion", "correlation")){

        if(!all(part %in% c("selection", "outcome", "dispersion", "correlation"))) {
                stop("part must be 'selection', 'outcome', 'dispersion', or 'correlation'")
        }

        if(length(part) > 1) {
                part = 'outcome'
        }

        if(part == "selection"){
                fitted = object$fitted.values$fit.selection
        }

        if(part == "outcome"){
                fitted = object$fitted.values$fit.outcome
        }

        if(part == "dispersion"){
                fitted = object$fitted.values$fit.dispersion
        }

        if(part == "correlation"){
                fitted = object$fitted.values$fit.correlation
        }

        return(fitted)
}
