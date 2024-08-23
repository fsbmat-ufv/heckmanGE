#' model.frame.fastHeckmanGe
#'
#' Get the Model Frames of a Generalized Heckman Regression
#'
#' @param object fastHeckmanGe class object.
#' @param part A character vector with the model part names: selection, outcome (default), dispersion, correlation.
#' @return
#' A model frame of (part of) a fastHeckmanGe object.
#' @export
model.frame.fastHeckmanGe = function(object,
                                      part = c("selection", "outcome", "dispersion", "correlation")){

        if(!all(part %in% c("selection", "outcome", "dispersion", "correlation"))) {
                stop("part must be 'selection', 'outcome', 'dispersion', or 'correlation'")
        }

        if(length(part) > 1) {
                part = 'outcome'
        }

        object$model.frames[[paste0("model.frame.",part)]]
}
