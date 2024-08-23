#' sandwich.fastHeckmanGe
#'
#' @export
sandwich.fastHeckmanGe = function (x,
                                   bread. = bread.fastHeckmanGe,
                                   meat.  = meat.fastHeckmanGe,
                                   ...){

        if (is.list(x) && !is.null(x$na.action))
                class(x$na.action) <- "omit"

        if (is.function(bread.))
                bread. <- bread.(x)

        if (is.function(meat.))
                meat. <- meat.(x, ...)

        n <- NROW(estfun.fastHeckmanGe(x))
        return(1/n * (bread. %*% meat. %*% bread.))
}
