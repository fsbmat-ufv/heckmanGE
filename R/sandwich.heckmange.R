#' sandwich.heckmange
#' Sandwich Estimator for Generalized Heckman Model
#'
#' Computes the sandwich estimator of the variance-covariance matrix for the `heckmange` model.
#'
#' @param x An object of class `heckmange` representing a fitted Generalized Heckman model.
#' @param bread. A function to compute the "bread" part of the sandwich estimator. Defaults to `bread.heckmange`.
#' @param meat. A function to compute the "meat" part of the sandwich estimator. Defaults to `meat.heckmange`.
#' @param ... Additional arguments passed to `meat.`.
#' @return
#' A variance-covariance matrix for the `heckmange` model, computed using the sandwich estimator.
#' @export
sandwich.heckmange = function (x,
                                   bread. = bread.heckmange,
                                   meat.  = meat.heckmange,
                                   ...){

        if (is.list(x) && !is.null(x$na.action))
                class(x$na.action) <- "omit"

        if (is.function(bread.))
                bread. <- bread.(x)

        if (is.function(meat.))
                meat. <- meat.(x, ...)

        n <- NROW(estfun.heckmange(x))
        return(1/n * (bread. %*% meat. %*% bread.))
}
