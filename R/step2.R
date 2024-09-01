#' step2: Two-Step Estimation Function
#'
#' This function performs a two-step estimation process, commonly used in models that require correction for sample selection bias. It estimates parameters for selection, outcome, and dispersion equations.
#'
#' @param YS A binary vector indicating selection (1 if selected, 0 otherwise).
#' @param XS A matrix of covariates for the selection equation.
#' @param YO A vector of observed outcomes for the selected sample.
#' @param XO A matrix of covariates for the outcome equation.
#' @param Msigma A matrix of covariates for the dispersion equation.
#' @param Mrho A matrix of covariates for the correlation structure.
#' @param w A vector of weights to be used in the estimation process.
#'
#' @return A list containing the estimated coefficients for the selection, outcome, dispersion, and correlation equations.
#'
#' @details
#' The function first estimates the selection equation using a probit model. The inverse Mills ratio (IMR) is calculated and included as an additional covariate in the outcome and dispersion equations. The outcome equation is estimated using weighted least squares, and the residuals are used to estimate the dispersion equation. Initial guesses for the correlation structure are also provided.
#'
#' @importFrom stats glm.fit dnorm pnorm
#'
#' @export
step2 <- function(YS, XS, YO, XO, Msigma, Mrho, w) {

        # Two-step estimation

        w1 = w[YS == 1]
        C_Cdqrls <- getNativeSymbolInfo("Cdqrls", PACKAGE = getLoadedDLLs()$stats)
        # Selection equation
        fit1 = glm.fit(y = YS, x = XS, weights = w, family = binomial(link = "probit"))
        IMR  <- dnorm(fit1$linear.predictors)/pnorm(fit1$linear.predictors)
        xMat <- cbind(XO, IMR)
        guess_coef_selection = coef(fit1)

        # Outcome Equation
        fit2 <- .Call(C_Cdqrls, xMat[YS == 1, ] * sqrt(w1), YO[YS == 1] * sqrt(w1), 1e-08, FALSE)
        fit2$residuals = fit2$residuals/sqrt(w1)
        fit2$fitted.values = YO[YS == 1] - fit2$residuals
        names(fit2$coefficients) = colnames(xMat)
        xMat <- data.frame(xMat)
        guess_coefs_outcome = coef(fit2)[!names(coef(fit2)) %in% "IMR"]

        # Dispersion equation
        sd_hat = sqrt( (fit2$residuals)^2 )
        Msigma_IMR <- cbind(Msigma, IMR)
        fit3 <- .Call(C_Cdqrls, Msigma_IMR[YS == 1, ] * sqrt(w1), log(sd_hat) * sqrt(w1), 1e-08, FALSE)
        names(fit3$coefficients) = colnames(Msigma_IMR)
        guess_coef_dispersion = coef(fit3)[!names(coef(fit3)) %in% "IMR"]

        # Geracao de valores da nova covariavel delta
        delta <- (xMat$IMR) * (xMat$IMR + fit1$fitted.values)

        # Calculo da variancia de Y1
        Var   <- (sum(w1 * (fit2$residuals^2)) + ((coef(fit2)["IMR"]^2) * sum(w1 * delta[YS == 1])))/sum(w * YS) # ESSE TRECHO ESTÁ PROBLEMÁTICO
        sigma <- sqrt(Var)
        names(sigma) <- "sigma"

        # Calculo da correlacao entre Y1 e Y2
        guess_coef_correlation <- 0 #( coef(fit2)["IMR"]/sigma)
        guess_coef_correlation <- c(guess_coef_correlation, rep(0, ncol(Mrho) - 1))
        names(guess_coef_correlation) <- colnames(Mrho)

        #Chute inicial para optim do modelo
        beta <- list(selection   = guess_coef_selection,
                     outcome     = guess_coefs_outcome,
                     dispersion  = guess_coef_dispersion,
                     correlation = guess_coef_correlation)

        return(beta)
}
