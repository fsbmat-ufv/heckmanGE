#' bread.fastHeckmanGe
#'
#'
#'
#' @export
bread.fastHeckmanGe <- function(object, ...) {

        object$nObs * object$vcov  # deveriamos usar nObs ou a soma dos pesos?
}
