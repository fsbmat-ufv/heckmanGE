#' meat.heckmanGE
#' Compute Meat of the Covariance Matrix for the Generalized Heckman Model
#'
#' This function calculates the "meat" of the covariance matrix for a `heckmanGE` model. The "meat" refers to the part of the covariance matrix that is not accounted for by the model's fixed components. This is typically used in conjunction with the "bread" component to form a robust covariance matrix estimator.
#'
#' @param x An object of class `heckmanGE`. This object should be a fitted model for which the covariance matrix is to be computed.
#' @param adjust A logical value indicating whether to apply a small-sample correction to the covariance matrix. If `TRUE`, the matrix is adjusted by multiplying it by `n / (n - k)`, where `n` is the number of observations and `k` is the number of parameters.
#' @param ... Additional arguments passed to `estfun.heckmanGE`, which computes the estimating functions used in the calculation.
#'
#' @return A matrix representing the "meat" of the covariance matrix. The dimensions and row/column names of the matrix correspond to the number of parameters in the model.
#'
#' @details
#' - The function calculates the covariance matrix based on the estimating functions obtained from `estfun.heckmanGE`.
#' - The "meat" is calculated as the cross-product of the estimating functions, divided by the number of observations. If `adjust` is `TRUE`, a small-sample correction is applied.
#'
#' @export
meat.heckmanGE = function (x, adjust = FALSE, ...) {

        if (is.list(x) && !is.null(x$na.action))
                class(x$na.action) <- "omit"

        psi <- estfun.heckmanGE(x, ...)

        k <- NCOL(psi)
        n <- NROW(psi)

        rval <- crossprod(as.matrix(psi))/n
        if (adjust)
                rval <- n/(n - k) * rval
        rownames(rval) <- colnames(rval) <- colnames(psi)
        return(rval)
}

