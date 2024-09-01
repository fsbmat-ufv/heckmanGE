#' predict.heckmanGE
#' Predictions from the Generalized Heckman Model
#'
#' Generates predictions from a fitted `heckmanGE` model. Predictions can be made on the scale of the linear predictors or on the scale of the response variable. The function can also return confidence intervals for the predictions if requested.
#'
#' @param object An object of class `heckmanGE`. This object should be a fitted model from which predictions will be made.
#' @param part A character vector specifying the model part for which to make predictions. Options include "selection", "outcome", "dispersion", and "correlation". The default is "outcome". If multiple parts are specified, only the "outcome" part will be used.
#' @param newdata Optionally, a data frame containing new data for making predictions. If omitted, the function uses the fitted linear predictors from the model object.
#' @param type The type of prediction required. The default is "link", which returns predictions on the scale of the linear predictors. If "response" is specified, predictions are returned on the scale of the response variable after applying the inverse link function.
#' @param cofint A logical indicating whether to return confidence intervals for the predictions. Default is FALSE.
#' @param confidence_level A numeric value specifying the confidence level for the confidence intervals if `cofint` is TRUE. Default is 0.95.
#' @return
#' A vector or matrix of predictions from the `heckmanGE` object, depending on the value of `cofint`. If `cofint` is TRUE, the function returns a matrix with the mean predicted value, and the lower and upper bounds of the confidence interval.
#'
#' @details
#' - The function first checks the validity of the `part` and `type` arguments.
#' - If `newdata` is provided, the function ensures it matches the variables and structure of the original model frame.
#' - Predictions can be on the link scale or the response scale, depending on the `type` argument.
#' - Confidence intervals are calculated if `cofint` is TRUE, using the standard errors derived from the model.
#'
#' @importFrom stats model.frame pnorm qnorm
#' @importFrom utils head
#' @importFrom vctrs vec_size
#' @export
predict.heckmanGE = function(object,
                                 part = c("selection", "outcome", "dispersion", "correlation"),
                                 newdata = NULL,
                                 type    = c("link", "response"),
                                 cofint = F,
                                 confidence_level = .95){

        if(!all(part %in% c("selection", "outcome", "dispersion", "correlation"))) {
                stop("part must be 'selection', 'outcome', 'dispersion', or 'correlation'")
        }

        if(length(part) > 1) {
                part = 'outcome'
        }

        if(!all(type %in% c("link", "response"))) {
                stop("type must be 'link' or 'response'")
        }

        if(length(type) > 1) {
                type = 'link'
        }

        if(is.null(newdata)){

                fitted = object$linear.prediction[[paste0("pred.",part)]]

        }else{

                mf = object$call
                m <- match(part, names(mf), 0)
                expr = parse(text = paste0("stats::model.frame(formula = ", as.character(mf[m]),", data = newdata, drop.unused.levels = TRUE, na.action = na.pass)"))

                # model.frame requires the parameter to be formula
                newdata_model.frame  <- eval(expr) #, envir = parent.frame())
                original_model.frame <- model.frame.heckmanGE(object = object, part = part)

                original_model.frame$`(weights)` <- NULL

                if(!all(names(original_model.frame) %in% names(newdata_model.frame))){
                        stop(paste("newdata should have the same variables as the original model frame for the",part,"model"))
                }else{
                        newdata_model.frame <- newdata_model.frame[, names(newdata_model.frame) %in% names(original_model.frame)]
                }


                if(!length(newdata_model.frame) == length(original_model.frame)){
                        stop(paste("newdata should have the same number of variables as the original model frame for the",part,"model"))
                }


                newdata_model.frame <- newdata_model.frame[, names(original_model.frame)]

                for(i in 1:ncol(newdata_model.frame)){
                        if("factor" %in% class(original_model.frame[[i]])){
                                newdata_model.frame[[i]]         = factor(newdata_model.frame[[i]])
                                class(newdata_model.frame[[i]])  = class(original_model.frame[[i]])
                                levels(newdata_model.frame[[i]]) = levels(original_model.frame[[i]])
                        }
                }

                X <- model.matrix(terms(original_model.frame), newdata_model.frame)

                coef = coef.heckmanGE(object = object, part = part)

                predicted = as.numeric(X %*% coef)

        }


        if(cofint == T){
                Sigma = vcov.heckmanGE(object, part)

                se = as.numeric(sapply(1:nrow(X), \(i){
                        sqrt(X[i, ] %*% Sigma %*% X[i, ])
                }))

                z = abs(qnorm((1 - confidence_level)/2))

                upper = predicted + z*se
                lower = predicted - z*se

                predicted = cbind(predicted = predicted,
                                  lower     = lower,
                                  upper     = upper)
        }


        if(type == "response"){

                if(part == "selection"){
                        predicted = pnorm(predicted)
                }

                if(part == "outcome"){
                        predicted = predicted
                }

                if(part == "dispersion"){
                        predicted = exp(predicted)
                }

                if(part == "correlation"){
                        predicted = tanh(predicted)
                }

        }

        return(predicted)
}
