#' Variância-Covariância com Correção por Agrupamento para Modelos Heckman
#'
#' A função `vcovCL.fastHeckmanGe` calcula a matriz de variância-covariância de um modelo Heckman,
#' aplicando uma correção por agrupamento (cluster). Isso é útil para obter estimativas robustas da
#' variância, especialmente quando há dependência dentro dos grupos.
#'
#' @param x Um objeto resultante da estimativa de um modelo Heckman usando o método `fastHeckmanGe`.
#' @param cluster Um vetor ou fator que identifica os agrupamentos (clusters) nos dados. Se NULL, assume
#' que não há agrupamento.
#' @param type Um caractere que especifica o tipo de correção por agrupamento a ser utilizada. Pode ser
#' `"HC0"`, `"HC1"`, `"HC2"`, ou `"HC3"`.
#' @param sandwich Um valor lógico. Se TRUE, a função aplica a estimativa sandwich à matriz de
#' variância-covariância.
#' @param fix Um valor lógico. Se TRUE, corrige eventuais valores negativos nos autovalores da matriz
#' de variância-covariância.
#' @param ... Argumentos adicionais que podem ser passados para métodos internos.
#'
#' @details Esta função é uma implementação especializada para a obtenção de uma matriz de
#' variância-covariância robusta a partir de modelos Heckman estimados com `fastHeckmanGe`. Ela permite
#' a aplicação de correção por agrupamento, o que é especialmente importante em contextos onde
#' observações dentro de grupos podem não ser independentes.
#'
#' @return Uma matriz de variância-covariância corrigida.
#'
#' @seealso [meatCL.fastHeckmanGe()], [sandwich.fastHeckmanGe()], [bread.fastHeckmanGe()]
#'
#' @export
vcovCL.fastHeckmanGe = function (x, cluster = NULL, type = NULL, sandwich = TRUE, fix = FALSE, ...) {


        rval <- meatCL.fastHeckmanGe(x, cluster = cluster, type = type, ...)
        #rval <- meatCL.fastHeckmanGe(x, cluster = cluster, type = type)

        if (sandwich)
                rval <- sandwich.fastHeckmanGe(x,
                                               bread. = bread.fastHeckmanGe,
                                               meat.  = rval)
        if (fix && any((eig <- eigen(rval, symmetric = TRUE))$values < 0)) {
                eig$values <- pmax(eig$values, 0)
                rval[] <- crossprod(sqrt(eig$values) * t(eig$vectors))
        }
        return(rval)
}
