#----------------------------------------------#
# Author: Antoine Mayerowitz and Maxime Gravoueille
# Date creation: 2021-07-25 18:28:19 CEST
# ~: function to perform the estimation
#----------------------------------------------#

#' Standard errors
#'
#' @param s Object of class "didImputation".
#' @param ... other arguments.
#'
SE <- function(s, ...) {
  dt <- s$data

  s$se <- sapply(s$weights_cols, computeWeightSE, dt, s, simplify = FALSE)

  return(s)
}

#' Weight specific standard error
#'
#' @param w_i Character. Name of the weight.
#' @param dt data.table. Estimation table.
#' @param s Object of class "didImputation".
#' @param ... other arguments.
#'
computeWeightSE <- function(w_i, dt, s, ...) {
  # Must declare NSE variables to avoid CRAN Check error
  .tau_mean <- NULL
  .tau <- NULL
  .eps <- NULL
  var <- NULL
  .d <- NULL
  . <- NULL

  group <- c(s$time, s$cohort)

  dt[.d == 1, .tau_mean := (sum(get(w_i)^2 * .tau)) / sum(get(w_i)^2), by = group]
  dt[is.nan(.tau_mean), .tau_mean := 0]
  dt[.d == 1, .eps := .tau - .tau_mean]
  se <- dt[, .(var = sum(get(w_i) * .eps)), by = c(s$unit)]
  as.numeric(se[, .(se = sqrt(sum(var^2)))])
}
