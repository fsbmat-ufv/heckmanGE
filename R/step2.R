#' step2
#'
#' @export
step2 <- function(YS, XS, YO, XO, Msigma, Mrho, w) {

        # Two-step estimation

        w1 = w[YS == 1]

        # Selection equation
        fit1 = glm.fit(y = YS, x = XS, weights = w, family = binomial(link = "probit"))
        IMR  <- dnorm(fit1$linear.predictors)/pnorm(fit1$linear.predictors)
        xMat <- cbind(XO, IMR)
        guess_coef_selection = coef(fit1)

        # Outcome Equation
        fit2 <- .Call(stats:::C_Cdqrls, xMat[YS == 1, ] * sqrt(w1), YO[YS == 1] * sqrt(w1), 1e-08, FALSE)
        fit2$residuals = fit2$residuals/sqrt(w1)
        fit2$fitted.values = YO[YS == 1] - fit2$residuals
        names(fit2$coefficients) = colnames(xMat)
        xMat <- data.frame(xMat)
        guess_coefs_outcome = coef(fit2)[!names(coef(fit2)) %in% "IMR"]

        # Dispersion equation
        sd_hat = sqrt( (fit2$residuals)^2 )
        Msigma_IMR <- cbind(Msigma, IMR)
        fit3 <- .Call(stats:::C_Cdqrls, Msigma_IMR[YS == 1, ] * sqrt(w1), log(sd_hat) * sqrt(w1), 1e-08, FALSE)
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
