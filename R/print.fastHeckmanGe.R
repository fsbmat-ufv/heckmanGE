#' print.fastHeckmanGe
#'
#' Print of Generalized Heckman Model results
#'
#' @return
#' Print estimates of the Generalized Heckman model
#' @param object fastHeckmanGe class object.
#' @param ... others functions.
#' @export print.fastHeckmanGe
#' @export
print.fastHeckmanGe  <- function(object, ... ){

        indexes       <- object$coefficients_indexes
        coefficients  <- object$coefficients

        loglik        <- object$loglik
        aic           <- object$aic
        bic           <- object$bic
        nObs          <- object$nObs
        N0            <- object$N0
        N1            <- object$N1
        nParam        <- object$nParam
        df            <- object$df

        cat("Generalized Heckman Model (Package: fastHeckmanGe)\n")
        cat("\n")
        cat("Call:\n")
        print(object$call)
        cat("\n")
        cat("Probit selection equation:\n")
        print(coefficients[indexes$index.selection])
        cat("\n")
        cat("Outcome equation:\n")
        print(coefficients[indexes$index.outcome])
        cat("\n")
        cat("Dispersion equation:\n")
        print(coefficients[indexes$index.dispersion])
        cat("\n")
        cat("Correlation equation:\n")
        print(coefficients[indexes$index.correlation])
        cat("\n")
        cat("Log-Likelihood:", loglik, "\n")
        cat("AIC:", aic, "BIC:", bic, "\n")
        cat("Number of observations:", nObs, "(", N0, "censored and", N1, "observed", ")", "\n")
        cat(nParam, "free parameters", "(", "df =", df, ")", "\n")
}
