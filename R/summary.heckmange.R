#' summary.heckmange
#' Summary of Generalized Heckman Model
#'
#' Provides a summary of the parameters and diagnostic information from a fitted Generalized Heckman model.
#'
#' @param object An object of class `heckmange` representing a fitted Generalized Heckman model.
#' @param ... Additional arguments passed to other methods.
#' @return
#' Prints a detailed summary of the fitted Generalized Heckman model, including parameter estimates, standard errors, model fit statistics, and optimization details.
#' @importFrom miscTools coefTable
#' @importFrom stats printCoefmat
#' @export
summary.heckmange  <- function(object, ... ){

        indexes       <- object$coefficients_indexes

        se            <- object$se
        coefficients  <- object$coefficients

        counts        <- object$counts
        loglik        <- object$loglik
        nObs          <- object$nObs
        weightedObs   <- object$weightedObs
        nParam        <- object$nParam
        df            <- object$df
        N0            <- object$N0
        N1            <- object$N1
        w0            <- object$w0
        w1            <- object$w1
        aic           <- object$aic
        bic           <- object$bic

        tb.selection <- miscTools::coefTable(coefficients[indexes$index.selection],
                                             se[indexes$index.selection],
                                             df = df)

        tb.outcome <- miscTools::coefTable(coefficients[indexes$index.outcome],
                                           se[indexes$index.outcome],
                                           df = df)

        tb.dispersion <- miscTools::coefTable(coefficients[indexes$index.dispersion],
                                              se[indexes$index.dispersion],
                                              df = df)

        tb.correlation <- miscTools::coefTable(coefficients[indexes$index.correlation],
                                               se[indexes$index.correlation],
                                               df = df)

        optim_function       = "maxLik::maxNR"
        optim_method         = object$optimization$method
        optim_iterations     = object$optimization$iterations
        optim_convergence    = object$optimization$convergence_code
        optim_message        = object$optimization$message

        cat("Generalized Heckman Model (Package: heckmange)\n")
        cat("--------------------------------------------------\n")
        cat("\n")
        cat("Call:\n")
        print(object$call)
        cat("\n\n")
        cat("Probit selection equation:\n")
        printCoefmat(tb.selection, signif.stars = TRUE, signif.legend = FALSE, digits = 4)
        cat("\n\n")
        cat("Outcome equation:\n")
        printCoefmat(tb.outcome, signif.stars = TRUE, signif.legend = FALSE, digits = 4)
        cat("\n\n")
        cat("Dispersion equation:\n")
        printCoefmat(tb.dispersion, signif.stars = TRUE, signif.legend = FALSE, digits = 4)
        cat("\n\n")
        cat("Correlation equation:\n")
        printCoefmat(tb.correlation, signif.stars = TRUE, signif.legend = TRUE, digits = 4)
        cat("\n")
        cat("---\n")
        cat("Maximum Likelihood estimation \n")
        cat(paste(optim_function, "function with method", optim_method, "\n"))
        cat(paste("Number of iterations:", optim_iterations, "\n"))
        cat(paste("Convergence code:", optim_convergence, "\n"))
        cat(paste("Convergence message:", optim_message, "\n"))
        cat("Log-Likelihood:", loglik, "\n")
        cat("AIC:", aic, "BIC:", bic, "\n")
        cat("Number of observations:", nObs, "(", N0, "censored and", N1, "observed", ")", "\n")
        cat("Weighted number of observations:", weightedObs, "(", w0, "censored and", w1, "observed", ")", "\n")

        cat("Standard Error type:", object$se.type, "\n")
        if(object$se.type == "clustered"){
                cat("Cluster variables:", object$cluster_vars, "\n")
        }

        cat(nParam, "free parameters", "(", "df =", df, ")", "\n")
        invisible(object)
}



