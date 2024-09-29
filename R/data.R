#' heckmanGE: A Package for Fitting Sample Selection Models
#'
#' This package provides functions for fitting sample selection models, specifically the Heckman-Ge model. It includes functionality for specifying selection and outcome equations, as well as adjusting parameters for dispersion and correlation.
#'
#' @return A list of results from the fitted model, including parameter estimates, the Hessian matrix, number of observations, and other relevant statistics. If initial values are not provided, the function estimates them using the Heckman two-step method.
#'
#' @author Fernando de Souza Bastos
#'
#' @keywords Heckman, Sample Selection
#'
#' @name heckmanGE
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

#' PNAD Continua de 2024, 2 trimestre
#'
#' The Continuous National Household Sample Survey (PNAD Continua)
#' for the second quarter of 2024 is an important source of
#' statistical data in Brazil, conducted by the Brazilian Institute
#' of Geography and Statistics (IBGE). The survey aims to provide
#' up-to-date information on the socioeconomic characteristics of
#' the Brazilian population, covering topics such as employment,
#' income, education, and other crucial aspects for the formulation
#' of public policies and economic and social studies.
#'
#' @format A data frame with 326018 observations on the following variables:
#' \itemize{

#'   \item{PSU: Primary Sampling Unit identifier (factor)}
#'   \item{weight: Survey weight (numeric)}
#'   \item{age: Age of the respondent (numeric)}
#'   \item{participation: Labor force participation status (factor)}
#'   \item{male: Male indicator (binary)}
#'   \item{white: White indicator (binary)}
#'   \item{hhold_head: Household head indicator (binary)}
#'   \item{hhold_spouse: Spouse of household head indicator (binary)}
#'   \item{yearsSchooling: Total years of schooling completed (numeric)}
#'   \item{classWorker_employer: Employer indicator (binary)}
#'   \item{classWorker_selfEmployed: Self-employed indicator (binary)}
#'   \item{ln_salary: Natural logarithm of salary (numeric)}
#' }
#'
#' @examples
#' data(pnadC_y2024q2)
#' pnadC_y2024q2 <- pnadC_y2024q2[1:10000,]
#' attach(pnadC_y2024q2)
#'selectEq  <- participation ~ age + I(age^2) +
#'  male + white + yearsSchooling +
#'  hhold_head + hhold_spouse
#'outcomeEq <- ln_salary ~ age + I(age^2) +
#'  male + white + yearsSchooling +
#'  classWorker_employer + classWorker_selfEmployed
#'outcomeD  <- ~ age + I(age^2) +
#'  male + white + yearsSchooling +
#'  classWorker_employer + classWorker_selfEmployed
#' outcomeC  <- ~ male + yearsSchooling
#' fit_heckmanGE <- heckmanGE(selection   = selectEq,
#'                            outcome     = outcomeEq,
#'                            dispersion  = outcomeD,
#'                            correlation = outcomeC,
#'                            data = pnadC_y2024q2,
#'                            weights = weight,
#'                            cluster = ~PSU)
#'                            summary(fit_heckmanGE)
"pnadC_y2024q2"

#' Simulation dataset for the heckmanGE example
#'
#' This dataset contains simulated data used to illustrate the functionality
#' of the heckmanGE model. The data includes variables used in selection, outcome,
#' dispersion, and correlation equations.
#'
#' @format A data frame with 10,000 observations on the following variables:
#' \itemize{
#'   \item{y_o: Outcome variable from the simulated model (numeric)}
#'   \item{y_s: Selection indicator, 1 if selected, 0 otherwise (binary)}
#'   \item{prob_s: Probability of selection (numeric)}
#'   \item{x1: Simulated predictor from a normal distribution (numeric)}
#'   \item{x2: Simulated predictor from a Poisson distribution (numeric)}
#'   \item{x3: Simulated binary predictor (binary)}
#'   \item{x4: Simulated predictor from a normal distribution with mean 2 and sd 2 (numeric)}
#'   \item{x5: Simulated predictor from a Poisson distribution with lambda 1.5 (numeric)}
#' }
#'
#' @examples
#' data(simulation)
#' selectEq  <- y_s ~ x1 + x2 + x4
#' outcomeEq <- y_o ~ x1 + x2 + x3
#' outcomeD  <- ~ x1 + x5
#' outcomeC  <- ~ x3 + x4
#' fit_heckmanGE <- heckmanGE(selection   = selectEq,
#'                            outcome     = outcomeEq,
#'                            dispersion  = outcomeD,
#'                            correlation = outcomeC,
#'                            data = simulation)
#' summary(fit_heckmanGE)
"simulation"


.onAttach <- function(libname, pkgname) {
  packageStartupMessage("If you have questions, suggestions, or comments regarding the 'heckmanGE' package, please contact Fernando de Souza Bastos at fernando.bastos@ufv.br.")
}
