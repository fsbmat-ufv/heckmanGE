#' print.heckmange
#' Print of Generalized Heckman Model Results
#'
#' Prints a summary of the results from a fitted `heckmange` model, including estimates for different model components, log-likelihood, AIC, BIC, and other relevant statistics.
#'
#' @param x An x of class `heckmange`. This x should be a fitted model whose results are to be printed.
#' @param ... Additional arguments passed to or from other methods.
#' @return
#' Prints the estimates and statistics of the Generalized Heckman model to the console.
#' @importFrom stats coef logLik
#' @importFrom utils capture.output
#' @export
print.heckmange  <- function(x, ... ){

        indexes       <- x$coefficients_indexes
        coefficients  <- x$coefficients

        loglik        <- x$loglik
        aic           <- x$aic
        bic           <- x$bic
        nObs          <- x$nObs
        N0            <- x$N0
        N1            <- x$N1
        nParam        <- x$nParam
        df            <- x$df

        cat("Generalized Heckman Model (Package: heckmange)\n")
        cat("\n")
        cat("Call:\n")
        print(x$call)
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
        invisible(x)
}



