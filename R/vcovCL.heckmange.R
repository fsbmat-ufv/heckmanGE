#' Variance-Covariance with Cluster Correction for Heckman Models
#'
#' The `vcovCL.heckmanGE` function computes the variance-covariance matrix of a Heckman model,
#' applying a cluster correction. This is useful for obtaining robust variance estimates, especially
#' when there is within-group dependence.
#'
#' @param x An object resulting from the estimation of a Heckman model using the `heckmanGE` method.
#' @param cluster A vector or factor identifying clusters in the data. If NULL, assumes no clustering.
#' @param type A character string specifying the type of cluster correction to be applied. It can be
#' `"HC0"`, `"HC1"`, `"HC2"`, or `"HC3"`.
#' @param sandwich A logical value. If TRUE, the function applies the sandwich estimator to the
#' variance-covariance matrix.
#' @param fix A logical value. If TRUE, corrects any negative eigenvalues in the variance-covariance matrix.
#' @param ... Additional arguments that can be passed to internal methods.
#'
#' @details This function is a specialized implementation for obtaining a robust variance-covariance matrix
#' from Heckman models estimated with `heckmanGE`. It allows for cluster correction, which is particularly
#' important in contexts where observations within groups may not be independent.
#'
#' @return A corrected variance-covariance matrix.
#'
#' @seealso [meatCL.heckmanGE()], [sandwich.heckmanGE()], [bread.heckmanGE()]
#'
#' @importFrom stats complete.cases pnorm dnorm binomial coef glm.fit model.matrix model.response model.weights na.pass terms
#' @importFrom utils tail
#' @export
vcovCL.heckmanGE = function (x, cluster = NULL, type = NULL, sandwich = TRUE, fix = FALSE, ...) {
  rval <- meatCL.heckmanGE(x, cluster = cluster, type = type, ...)
  #rval <- meatCL.heckmanGE(x, cluster = cluster, type = type)

  if (sandwich)
    rval <- sandwich.heckmanGE(x,
                               bread. = bread.heckmanGE,
                               meat.  = rval)
  if (fix && any((eig <- eigen(rval, symmetric = TRUE))$values < 0)) {
    eig$values <- pmax(eig$values, 0)
    rval[] <- crossprod(sqrt(eig$values) * t(eig$vectors))
  }
  return(rval)
}
