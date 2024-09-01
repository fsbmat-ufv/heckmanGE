#' heckmange: A Package for Fitting Sample Selection Models
#'
#' This package provides functions for fitting sample selection models, specifically the Heckman-Ge model. It includes functionality for specifying selection and outcome equations, as well as adjusting parameters for dispersion and correlation.
#'
#' @return A list of results from the fitted model, including parameter estimates, the Hessian matrix, number of observations, and other relevant statistics. If initial values are not provided, the function estimates them using the Heckman two-step method.
#'
#' @author Fernando de Souza Bastos
#'
#' @keywords Heckman, Sample Selection
#'
#' @name heckmange
NULL

#' Medical Expenditure Panel Survey (MEPS) Data
#'
#' The MEPS dataset contains large-scale survey data from the United States, focusing on health services usage, costs, and insurance coverage. This dataset is restricted to individuals aged 21 to 64 years. It includes outpatient cost data with some zero expenditure values for model adjustment.
#'
#' @format A data frame with 3328 observations on the following variables:
#' \itemize{
#'   \item{educ: Education status (numeric)}
#'   \item{age: Age (numeric)}
#'   \item{income: Income (numeric)}
#'   \item{female: Gender (binary)}
#'   \item{vgood: Self-reported health status, very good (numeric)}
#'   \item{good: Self-reported health status, good (numeric)}
#'   \item{hospexp: Hospital expenditures (numeric)}
#'   \item{totchr: Total number of chronic diseases (numeric)}
#'   \item{ffs: Family support (numeric)}
#'   \item{dhospexp: Dummy variable for hospital expenditures (binary)}
#'   \item{age2: Age squared (numeric)}
#'   \item{agefem: Interaction between age and gender (numeric)}
#'   \item{fairpoor: Self-reported health status, fair or poor (numeric)}
#'   \item{year01: Year of survey (numeric)}
#'   \item{instype: Type of insurance (numeric)}
#'   \item{ambexp: Ambulatory expenditures (numeric)}
#'   \item{lambexp: Log of ambulatory expenditures (numeric)}
#'   \item{blhisp: Ethnicity (binary)}
#'   \item{instype_s1: Insurance type, version 1 (numeric)}
#'   \item{dambexp: Dummy variable for ambulatory expenditures (binary)}
#'   \item{lnambx: Log-transformed ambulatory expenditures (numeric)}
#'   \item{ins: Insurance status (binary)}
#' }
#'
#' @source 2001 Medical Expenditure Panel Survey by the Agency for Healthcare Research and Quality.
#'
#' @examples
#' data(MEPS2001)
#' attach(MEPS2001)
#' hist(lnambx)
#' selectEq <- dambexp ~ age + female + educ + blhisp + totchr + ins + income
#' outcomeEq <- lnambx ~ age + female + educ + blhisp + totchr + ins
"MEPS2001"

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("If you have questions, suggestions, or comments regarding the 'heckmange' package, please contact Fernando de Souza Bastos at fernando.bastos@ufv.br.")
}
