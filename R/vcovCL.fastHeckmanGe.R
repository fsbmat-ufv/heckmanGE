#' vcovCL.fastHeckmanGe
#'
#' @export
vcovCL.fastHeckmanGe = function (x, cluster = NULL, type = NULL, sandwich = TRUE, fix = FALSE, ...) {


        rval <- meatCL.fastHeckmanGe(x, cluster = cluster, type = type, ...)
        #rval <- meatCL.fastHeckmanGe(x, cluster = cluster, type = type)

        if (sandwich)
                rval <- sandwich.fastHeckmanGe(x,
                                               bread. = bread.fastHeckmanGe,
                                               meat.  = rval)
        if (fix && any((eig <- eigen(rval, symmetric = TRUE))$values < 0)) {
                eig$values <- pmax(eig$values, 0)
                rval[] <- crossprod(sqrt(eig$values) * t(eig$vectors))
        }
        return(rval)
}
