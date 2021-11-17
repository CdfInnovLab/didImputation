#----------------------------------------------#
# Author: Antoine Mayerowitz and Maxime Gravoueille
# Date creation: Tue May 25 11:30:53 2021
# ~: function to perform pre-trend test
#----------------------------------------------#
# TODO:
#   - Compute the right SE


#' Test of parallel trends
#'
#' @param s an object of class "didImputation".
#' @param ... additional arguments.
#'
#' @return A fixest regression result with pre-trend coefficients.
#'
#' @importFrom stats as.formula
#'
preTrend <- function(s, ...) {
  .d <- NULL

  leads <- s$coef[1]

  if(s$ncontrasts == 1) {
    reg <- paste0("~ i(.k,  keep = -1:leads ) + ")
  } else {
    reg <- paste0("~ i(.k, i.", s$het,", keep = -1:leads ) + ")
  }

  pre_formula <- as.formula(gsub("~ ", reg, deparse(s$y0)))

  # Run TWFE regression with leads on untreated observations.
  reg_pre <- fixest::feols(
    pre_formula,
    s$data[.d == 0]
  )

  s$pre_trends <- reg_pre

  s$wald <- fixest::fitstat(reg_pre, "wald")
  return(s)
}
