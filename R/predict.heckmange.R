#' predict.heckmanGE
#' Predictions from the Generalized Heckman Model
#'
#' Generates predictions from a fitted `heckmanGE` model. Predictions can be made on the scale of the linear predictors or on the scale of the response variable. The function can also return confidence intervals for the predictions if requested.
#'
#' @param object An object of class `heckmanGE`. This object should be a fitted model from which predictions will be made.
#' @param \dots Additional arguments passed to specific methods. These are kept for compatibility with the generic `predict` function.
#' @param part A character vector specifying the model part for which to make predictions. Options include "selection", "outcome", "dispersion", and "correlation". Default is "outcome".
#' @param newdata Optionally, a data frame containing new data for making predictions. If omitted, the function uses the fitted linear predictors from the model object.
#' @param type The type of prediction required. Default is "link", which returns predictions on the scale of the linear predictors. If "response" is specified, predictions are on the scale of the response variable.
#' @param cofint A logical indicating whether to return confidence intervals for the predictions. Default is FALSE.
#' @param confidence_level A numeric value specifying the confidence level for the confidence intervals if `cofint` is TRUE. Default is 0.95.
#' @return A vector or matrix of predictions. If `cofint` is TRUE, returns a matrix with predicted values and confidence intervals.
#' @importFrom stats qnorm
#' @export
predict.heckmanGE = function(object, ...,
                             part = c("selection", "outcome", "dispersion", "correlation"),
                             newdata = NULL,
                             type    = c("link", "response"),
                             cofint = FALSE,
                             confidence_level = 0.95) {

  # Validar argumentos
  part <- match.arg(part)
  type <- match.arg(type)

  # Inicializar variável para predições
  predicted <- NULL

  # Se `newdata` não for fornecido, usar predições ajustadas
  if (is.null(newdata)) {
    if (!is.null(object$linear.prediction[[paste0("pred.", part)]])) {
      predicted <- object$linear.prediction[[paste0("pred.", part)]]
    } else {
      stop(paste("No fitted predictions found for part:", part))
    }
  } else {
    # Processar `newdata` para calcular predições
    tryCatch({
      original_model.frame <- model.frame.heckmanGE(object, part = part)
      terms_obj <- terms(original_model.frame)
      X <- model.matrix(terms_obj, newdata)
    }, error = function(e) {
      stop("Error in processing `newdata`. Ensure it matches the structure of the original model frame.")
    })

    # Garantir que coeficientes existam
    coef <- coef.heckmanGE(object = object, part = part)
    if (is.null(coef)) {
      stop("Coefficients not found for the specified model part.")
    }

    # Calcular predições
    predicted <- as.numeric(X %*% coef)
  }

  # Adicionar intervalos de confiança, se necessário
  if (cofint) {
    Sigma <- vcov.heckmanGE(object, part)
    se <- sqrt(rowSums((X %*% Sigma) * X))
    z <- qnorm(1 - (1 - confidence_level) / 2)
    predicted <- cbind(
      predicted = predicted,
      lower = predicted - z * se,
      upper = predicted + z * se
    )
  }

  # Transformar predições para escala da resposta
  if (type == "response") {
    transform_fn <- switch(part,
                           selection = pnorm,
                           dispersion = exp,
                           correlation = tanh,
                           outcome = identity
    )
    predicted <- transform_fn(predicted)
  }

  return(predicted)
}
