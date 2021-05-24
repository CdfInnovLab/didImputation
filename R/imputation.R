#----------------------------------------------#
# Author: Antoine Mayerowitz and Maxime Gravoueille
# Date creation: Mon May 24 15:17:00 2021
# ~: run imputation estimation
#----------------------------------------------#

#' Imputation estimation.
#'
#' @param s an object of class "didImputation"
#' @param ... additional arguments.
#'
#' @import data.table fixest
#' @importFrom stats resid predict
#' @return an object of class "didImputation" mutated with imputed values.
#'
runImputation <- function(s, ...) {
  eps <- NULL
  resid <- NULL
  cf <- NULL
  tau <- NULL

  # split treat/non-treated populations
  data <- s$data
  treated <- data[d == 1, ]
  ntreated <- data[d == 0, ]

  # run model used for imputation on non-treated population
  model <- feols(s$y0, ntreated, fixef.rm = "none")
  s$counterfactual <- model

  # predict counterfactual outcome
  data <- data[d == 0, eps := resid(model)]
  counterfactual <- predict(model, newdata = treated)
  data <- data[d == 1, cf := counterfactual]
  ndata <- nrow(data)
  data <- data[d == 0 | !is.na(cf)]

  if (nrow(data) != ndata) {
    warning(paste0(ndata - nrow(data), " observation dropped due to imputation."))
  }


  # compute treatment effect by unit
  data[, tau := get(s$y) - cf]
  s$data <- data

  return(s)
}
