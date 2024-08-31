#' fitfastHeckmanGe
#'
#' Optimized Function for Fitting the Generalized Heckman Model
#'
#' This function estimates the parameters of a generalized Heckman selection model using a Newton-Raphson optimization algorithm. It supports the modeling of selection and outcome equations, along with associated dispersion and correlation structures.
#'
#' @param start A numeric vector with initial parameter values for the model.
#' @param YS A binary vector indicating the selection equation outcomes (1 for selected, 0 for not selected).
#' @param XS A matrix of predictors for the selection equation.
#' @param YO A vector of observed outcomes for the outcome equation.
#' @param XO A matrix of predictors for the outcome equation.
#' @param Msigma A matrix related to the dispersion parameter.
#' @param Mrho A matrix related to the correlation parameter.
#' @param w A numeric vector of weights for the observations.
#'
#' @return A list containing:
#' \describe{
#'   \item{coefficients}{Estimated coefficients for the selection, outcome, dispersion, and correlation equations.}
#'   \item{fitted.values}{Fitted values for the selection, outcome, dispersion, and correlation equations.}
#'   \item{residuals}{Residuals for the selection and outcome equations.}
#'   \item{loglik}{The log-likelihood of the fitted model.}
#'   \item{vcov}{The variance-covariance matrix of the estimated parameters.}
#'   \item{aic}{The Akaike Information Criterion (AIC) for the model.}
#'   \item{bic}{The Bayesian Information Criterion (BIC) for the model.}
#'   \item{optimization}{Details of the optimization process, including the convergence status.}
#' }
#'
#' @details
#' The function optimizes the log-likelihood of a generalized Heckman selection model using a Newton-Raphson algorithm. The model allows for estimation of selection bias in the outcome equation, and incorporates additional parameters for dispersion and correlation.
#'
#' @export
fitfastHeckmanGe = function(start,
                            YS,
                            XS,
                            YO,
                            XO,
                            Msigma,
                            Mrho,
                            w){


        # Parameter indices ----

        NXS <- ncol(XS)
        NXO <- ncol(XO)
        NE  <- ncol(Msigma)
        NV  <- ncol(Mrho)

        istartS <- 1:NXS
        istartO <- seq(tail(istartS, 1) + 1, length = NXO)
        ilambda <- seq(tail(istartO, 1) + 1, length = NE)
        ikappa  <- seq(tail(ilambda, 1) + 1, length = NV)

        # Matrices for the complete and censored data  ----
        XS0 <- XS[YS == 0, , drop = FALSE]
        XS1 <- XS[YS == 1, , drop = FALSE]
        YO[is.na(YO)] <- 0
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

        # Functions ------------------------------------------------------------

        sech = function(z) 1/cosh(z)

        # Log-likelihood ----
        loglik_gen <- function(start) {

                # Extract parameters
                g      <- start[istartS]
                b      <- start[istartO]
                lambda <- start[ilambda]
                kappa  <- start[ikappa]

                # Compute mu1 and mu2
                mu2_0 <- XS0  %*% g
                mu2_1 <- XS1  %*% g
                mu1   <- XO1  %*% b

                # Compute sigma and rho
                #sigma <-  exp(Msigma  %*% lambda)
                #rho   <- tanh(Mrho    %*% kappa)
                sigma <-  exp(ES1  %*% lambda)
                rho   <- tanh(VS1  %*% kappa)

                # Compute log-likelihood terms
                z      <- (YO1 - mu1) /sigma
                r      <- sqrt(1 - rho^2)
                A_rho  <- 1/r
                A_rrho <- rho/r
                zeta   <- mu2_1 * A_rho + z * A_rrho

                # Calculate log-likelihood
                #ll     <-
                sum(w0 * pnorm(-mu2_0, log.p = TRUE))  +                                  # (YS == 0)
                        sum(w1 * (dnorm(z, log = TRUE) - log(sigma) + pnorm(zeta, log.p = TRUE))) # (YS == 1)
                #ll
        }

        # Gradient ----
        gradlik_gen <- function(start) {

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

                colSums(w*gradient)
        }


        # Hessian ----
        hessian_gen = function(start){

                ## parameter indices
                g      <- start[istartS]
                b      <- start[istartO]
                lambda <- start[ilambda]
                kappa  <- start[ikappa]

                mu20 <- as.numeric(XS0  %*% g)
                mu21 <- as.numeric(XS1  %*% g)
                mu11 <- as.numeric(XO1  %*% b)

                #sigma0 <- exp(as.numeric(ES0  %*% lambda))
                sigma1 <- exp(as.numeric(ES1  %*% lambda))

                #rho0 <- tanh(as.numeric(VS0  %*% kappa))
                rho1 <- tanh(as.numeric(VS1  %*% kappa))

                z      <- (YO1 - mu11)/sigma1
                A_rho  <- 1/sqrt(1 - rho1^2)
                A_rrho <- rho1*A_rho

                zeta <- (mu21 * A_rho + z * A_rrho)

                #MZeta <- exp(dnorm(zeta, log = TRUE) - pnorm(zeta, log.p = TRUE))
                MZeta  <- dnorm(zeta)/pnorm(zeta)
                MMZeta <- MZeta*(zeta+MZeta)

                #Mmu2 <- exp(dnorm(-mu20, log = TRUE) - pnorm(-mu20, log.p = TRUE))
                Mmu2 <- dnorm(-mu20)/pnorm(-mu20)

                dZetaRho <- A_rho*(mu21*A_rho*A_rrho+z*(1+(A_rrho^2)))

                #d2ZetaRho <- (A_rho^3)*(mu21+3*rho1*z)+(A_rho^5)*(3*mu21*(rho1^2)+3*z*rho1^3)
                d2ZetaRho = (A_rho^3)*(2*z*rho1 + (mu21 + z*rho1)*(1 + 3*(A_rrho^2)))

                dMzetaRho <- -MMZeta*dZetaRho
                Q_rho   <- (A_rho^3)*(rho1*mu21+z)

                #dQ_rho  <- 3*rho1*(A_rho^5)*(rho1*mu21+z)+(A_rho^3)*mu21
                dQ_rho  <- (A_rho^3)*(3*rho1*(A_rho^2)*(rho1*mu21 + z)+ mu21 )

                sechEta <- sech(as.numeric(VS1  %*% kappa))^2
                tgEta4  <- tanh(as.numeric(VS1  %*% kappa))

                # Starting the Hessian
                hessian                   <- matrix(0, length(start), length(start))

                hessian[istartS, istartS] <- -t(w0 * XS0)  %*% ((Mmu2*(Mmu2-mu20))*XS0) + t(w1 * XS1)  %*% (((-MZeta*(zeta+MZeta))*(A_rho^2))*XS1)

                hessian[istartO, istartS] <- -t(w1 * XO1)  %*% (((-MZeta*(zeta+MZeta))*((A_rho*A_rrho)/sigma1))*XS1)
                hessian[istartS, istartO] <- t(hessian[istartO, istartS])

                hessian[ilambda, istartS] <- -t(w1 *ES1) %*%((-A_rho*A_rrho*z*MZeta*(zeta+MZeta))*(XS1))
                hessian[istartS, ilambda] <- t(hessian[ilambda, istartS])

                hessian[ikappa, istartS] <- t(w1 *VS1)  %*% ((((rho1-((mu21*A_rrho+z*A_rho)*(zeta+MZeta)))*MZeta*(A_rho^3)*((sech(as.numeric(VS1  %*% kappa)))^2)))*XS1)
                hessian[istartS, ikappa] <- t(hessian[ikappa, istartS])

                hessian[istartO,istartO] <- -t(w1 *XO1)  %*% (((((A_rrho^2)*MZeta*(zeta+MZeta))+1)/(sigma1^2))*XO1)

                hessian[ilambda, istartO] <- t(w1 *ES1)  %*% (((((-A_rrho^2)*z*(MZeta*(zeta+MZeta))+MZeta*A_rrho-2*z)/sigma1)*XO1))
                hessian[istartO, ilambda] <- t(hessian[ilambda, istartO])

                hessian[ikappa, istartO]  <- t(w1 *VS1)  %*% ((((((mu21*(A_rrho^2)*(A_rho^2) + z*(A_rho^3)*A_rrho))*(zeta+MZeta))-A_rho*(1+A_rrho^2))*(MZeta)*(((sech(as.numeric(VS1  %*% kappa)))^2)/sigma1))*XO1)
                hessian[istartO,ikappa]   <- t(hessian[ikappa, istartO])

                hessian[ilambda, ilambda] <- -t(w1 *ES1)  %*% (((A_rrho^2)*(z^2)*MZeta*(zeta+MZeta)-MZeta*A_rrho*z+2*z^2) * ES1)

                hessian[ikappa, ilambda] <- t(w1 *VS1)  %*% (((mu21*(A_rrho^2)*(A_rho^2)+z*(A_rho^3)*A_rrho)*(zeta+MZeta)-A_rho*(1+A_rrho^2))*z*MZeta*((sech(as.numeric(VS1  %*% kappa)))^2)*ES1)
                hessian[ilambda, ikappa] <- t(hessian[ikappa, ilambda])

                hessian[ikappa, ikappa] <- t(w1 *VS1)  %*% ((((-dZetaRho^2)*MMZeta+MZeta*d2ZetaRho))*(sechEta^2)*VS1)+t(VS1)  %*% (((MZeta*dZetaRho))*(-2*(sechEta^2)*tgEta4*(1/(1-(rho1^2))))*VS1)

                hessian
        }


        # Optimization algorithm -----------------------------------------------

        theta_HG = maxLik::maxNR(fn   = loglik_gen,
                                 grad = gradlik_gen,
                                 hess = hessian_gen,
                                 start = start,
                                 finalHessian = T)

        # Gathering results ----------------------------------------------------

        optimum.value  <- theta_HG$maximum

        loglik        <-  optimum.value

        hessian       <- theta_HG$hessian
        fisher_infoHG <- -hessian
        level         <- levels(as.factor(YS))
        nObs          <- length(YS)
        N0            <- sum(YS == 0)
        N1            <- sum(YS == 1)
        weightedObs   <- sum(w)
        w0            <- sum(w0)
        w1            <- sum(w1)
        nParam        <- length(start)
        NXS           <- ncol(XS0)
        NXO           <- ncol(XO1)
        df            <- (nObs-nParam)
        aic           <- -2*loglik + 2*nParam
        bic           <- -2*loglik + nParam*log(nObs)

        vcov    <- solve(fisher_infoHG)
        se      <- sqrt(diag(vcov))
        se.type <- "iid"

        result <- list(coefficients  =  theta_HG$estimate,
                       coefficients_list = list(coef.selection   = theta_HG$estimate[istartS],
                                                coef.outcome     = theta_HG$estimate[istartO],
                                                coef.dispersion  = theta_HG$estimate[ilambda],
                                                coef.correlation = theta_HG$estimate[ikappa]),

                       coefficients_indexes = list(index.selection   = istartS,
                                                   index.outcome     = istartO,
                                                   index.dispersion  = ilambda,
                                                   index.correlation = ikappa),

                       model.responses = list(selection = YS,
                                              outcome   = ifelse(YS == 0, NA, YO)),

                       fitted.values = list(fit.selection   =  pnorm(as.numeric(XS   %*% theta_HG$estimate[istartS])),
                                            fit.outcome     =      as.numeric(XO     %*% theta_HG$estimate[istartO]),
                                            fit.dispersion  =  exp(as.numeric(Msigma %*% theta_HG$estimate[ilambda])),
                                            fit.correlation = tanh(as.numeric(Mrho   %*% theta_HG$estimate[ikappa]))),

                       linear.prediction = list(pred.selection   =  as.numeric(XS     %*% theta_HG$estimate[istartS]),
                                                pred.outcome     =  as.numeric(XO     %*% theta_HG$estimate[istartO]),
                                                pred.dispersion  =  as.numeric(Msigma %*% theta_HG$estimate[ilambda]),
                                                pred.correlation =  as.numeric(Mrho   %*% theta_HG$estimate[ikappa])),

                       residuals = list(residuals.selection = YS - as.numeric(XS %*% theta_HG$estimate[istartS]),
                                        residuals.outcome   = ifelse(YS == 0, NA, YO) - as.numeric(XO %*% theta_HG$estimate[istartO])),

                       weights = list(w = w,
                                      w1 = w1,
                                      w0 = w0),

                       model.matrices = list(X.selection   = XS,
                                             X.outcome     = XO,
                                             X.dispersion  = Msigma,
                                             X.correlation = Mrho),

                       optimization = list(optimum.value    = optimum.value,
                                           method           = "Newton-Raphson",
                                           initial.value    = start,
                                           iterations       = theta_HG$iterations,
                                           convergence_code = theta_HG$code,
                                           message          = theta_HG$message),

                       loglik        = loglik,
                       hessian       = hessian,
                       fisher_infoHG = fisher_infoHG,
                       vcov          = vcov,

                       se            = se,
                       se.type       = se.type,
                       cluster_vars  = NULL,

                       level         = level,
                       nObs          = length(YS),
                       N0            = sum(YS == 0),
                       N1            = sum(YS == 1),
                       weightedObs   = sum(w),
                       w0            = sum(w0),
                       w1            = sum(w1),
                       nParam        = length(start),
                       NXS           = NXS,
                       NXO           = NXO,
                       df            = df,
                       aic           = aic,
                       bic           = bic,

                       NE            = NE,
                       NV            = NV)


        result = structure(.Data = result,
                           class = "fitfastHeckmanGe")
        #class(result) <- "fastHeckmanGe"

        result
}
