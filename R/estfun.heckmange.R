#' Compute Estimating Functions for Generalized Heckman Model
#'
#' This function calculates the estimating functions (i.e., the gradient of the log-likelihood) for the Generalized Heckman model.
#' It is primarily used for model diagnostics and inference, providing the gradient for each observation with respect to model parameters.
#'
#' @param x An object of class `heckmanGE`, typically the result of fitting a generalized Heckman model. This object should contain model responses, model matrices, weights, and coefficient indexes necessary for the computation of the gradient.
#' @param ... Additional arguments (currently not used, reserved for future extensions).
#'
#' @details
#' The function computes the gradient of the log-likelihood function for the Generalized Heckman model, which includes the selection, outcome, dispersion, and correlation components.
#'
#' The gradient is calculated per observation, and internally, the helper function `gradlik_gen_i` computes the gradient for each observation given the model parameters. This involves extracting components such as model matrices, weights, and coefficient indexes, and performing matrix operations specific to the model's structure.
#'
#' @return A matrix of dimensions `n x p`, where `n` is the number of observations and `p` is the number of parameters in the model. Each element of the matrix corresponds to the gradient of the log-likelihood function with respect to a given parameter for each observation.
#'
#' @examples
#' # Assuming 'model' is a fitted object of class 'heckmanGE':
#' data(MEPS2001)
#' selectEq  <- dambexp ~ age + female + educ + blhisp + totchr + ins + income
#' outcomeEq <- lnambx ~ age + female + educ + blhisp + totchr + ins
#' dispersion  <- ~ age + female + totchr + ins
#' correlation  <- ~ age
#' fit <- heckmanGE(selection = selectEq,
#'                  outcome = outcomeEq,
#'                  dispersion = dispersion,
#'                  correlation = correlation,
#'                  data = MEPS2001)
#' estfun.heckmanGE(fit)
#'
#' @export
estfun.heckmanGE <- function(x, ...){

        YS = x$model.responses$selection
        YO = x$model.responses$outcome

        XS     = x$model.matrices$X.selection
        XO     = x$model.matrices$X.outcome
        Msigma = x$model.matrices$X.dispersion
        Mrho   = x$model.matrices$X.correlation
        w      = x$weights$w

        istartS = x$coefficients_indexes$index.selection
        istartO = x$coefficients_indexes$index.outcome
        ilambda = x$coefficients_indexes$index.dispersion
        ikappa  = x$coefficients_indexes$index.correlation

        # Matrices for the complete and censored data  ----
        XS0 <- XS[YS == 0, , drop = FALSE]
        XS1 <- XS[YS == 1, , drop = FALSE]

        YO1 <- YO[YS == 1]
        XO1 <- XO[YS == 1, , drop = FALSE]

        ES0 <- Msigma[YS == 0, , drop = FALSE]
        ES1 <- Msigma[YS == 1, , drop = FALSE]
        VS0 <- Mrho[YS == 0, , drop = FALSE]
        VS1 <- Mrho[YS == 1, , drop = FALSE]
        N0  <- sum(YS == 0)
        N1  <- sum(YS == 1)
        w0  <- w[YS == 0]
        w1  <- w[YS == 1]


        sech = function(z) 1/cosh(z)

        # Gradient for the i-th observation ----
        gradlik_gen_i <- function(start) {

                # Extract parameters
                g      <- start[istartS]
                b      <- start[istartO]
                lambda <- start[ilambda]
                kappa  <- start[ikappa]

                mu20 <- XS0  %*% g
                mu21 <- XS1  %*% g
                mu11 <- XO1  %*% b

                #sigma0 <- exp(ES0  %*% lambda)
                sigma1 <- exp(ES1  %*% lambda)

                #rho0 <- tanh(VS0  %*% kappa)
                rho1 <- tanh(VS1  %*% kappa)

                z      <- (YO1 - mu11)/sigma1
                r      <- sqrt(1 - rho1^2)
                A_rho  <- 1/r
                A_rrho <- rho1/r

                zeta   <- (mu21 * A_rho + z * A_rrho)
                MZeta  <- exp(dnorm(zeta, log = TRUE) - pnorm(zeta, log.p = TRUE))
                Mmu2   <- exp(dnorm(-mu20, log = TRUE) - pnorm(-mu20, log.p = TRUE))
                Q_rho  <- mu21 * rho1 * ((A_rho)^2) + z * (1 + A_rrho^2)
                Q_rrho <- mu21 * (1 + 2 * A_rrho^2) + 2 * z * rho1 * (1 + A_rrho^2)

                dim(z)      = NULL
                dim(Mmu2)   = NULL
                dim(MZeta)  = NULL
                dim(A_rho)  = NULL
                dim(A_rrho) = NULL
                dim(sigma1) = NULL
                dim(mu21)   = NULL
                dim(rho1)   = NULL

                gradient <- matrix(0, length(mu20) + length(mu21), length(start))
                gradient[YS == 0, istartS] <- - XS0 * Mmu2
                gradient[YS == 1, istartS] <- XS1 * MZeta * A_rho
                gradient[YS == 1, istartO] <- XO1 * (z - MZeta * A_rrho) * (1/sigma1)

                gradient[YS == 1, ilambda] <- ES1 * (z^2 - 1 - MZeta * z * A_rrho)

                gradient[YS == 1, ikappa] <- VS1 * MZeta * A_rho *
                        (sech(as.numeric(VS1  %*% kappa))^2) *
                        (mu21 * rho1 * (A_rho^2) + z * (1 + (A_rrho^2)))

                w*gradient
        }

        gradlik_gen_i(x$coefficients)
}
