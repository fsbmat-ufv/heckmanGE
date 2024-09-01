#' sandwich.heckmanGE
#' Sandwich Estimator for Generalized Heckman Model
#'
#' Computes the sandwich estimator of the variance-covariance matrix for the `heckmanGE` model.
#'
#' @param x An object of class `heckmanGE` representing a fitted Generalized Heckman model.
#' @param bread. A function to compute the "bread" part of the sandwich estimator. Defaults to `bread.heckmanGE`.
#' @param meat. A function to compute the "meat" part of the sandwich estimator. Defaults to `meat.heckmanGE`.
#' @param ... Additional arguments passed to `meat.`.
#' @return
#' A variance-covariance matrix for the `heckmanGE` model, computed using the sandwich estimator.
#' @export
sandwich.heckmanGE = function (x,
                                   bread. = bread.heckmanGE,
                                   meat.  = meat.heckmanGE,
                                   ...){

        if (is.list(x) && !is.null(x$na.action))
                class(x$na.action) <- "omit"

        if (is.function(bread.))
                bread. <- bread.(x)

        if (is.function(meat.))
                meat. <- meat.(x, ...)

        n <- NROW(estfun.heckmanGE(x))
        return(1/n * (bread. %*% meat. %*% bread.))
}
