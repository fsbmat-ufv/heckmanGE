#' predict.fastHeckmanGe
#'
#' Predictions from the Generalized Heckman Model
#'
#' @param object fastHeckmanGe class object.
#' @param part A character vector with the model part names: selection, outcome (default), dispersion, correlation.
#' @param newdata optionally, a data frame in which to look for variables with which to predict. If omitted, the fitted linear predictors are used.
#' @param type the type of prediction required. The default (link) is on the scale of the linear predictors; the alternative "response" is on the scale of the response variable after applying the inverse link function .
#' @param cofint A logical. If true, the function returns a matrix, with the mean predicted value, and the lower and upper bounds of the confidence interval.
#' @param confidence_level A numeric value. The confidence level for the Confidence Interval of the prediction (if cofint is set to TRUE). Default: 0.95.
#' @return
#' Predictions from fastHeckmanGe the object.
#' @export
predict.fastHeckmanGe = function(object,
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
                original_model.frame <- model.frame.fastHeckmanGe(object = object, part = part)

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

                coef = coef.fastHeckmanGe(object = object, part = part)

                predicted = as.numeric(X %*% coef)

        }


        if(cofint == T){
                Sigma = vcov.fastHeckmanGe(object, part)

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
