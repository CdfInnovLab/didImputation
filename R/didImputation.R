#----------------------------------------------#
# Author: Antoine Mayerowitz and Maxime Gravoueille
# Date creation: Mon May 24 15:17:00 2021
# ~: function to perform the estimation
#----------------------------------------------#

#' DiD regression with imputation method
#'
#' @param y0 Formula. model for Y(0). This is the full model without leads and lags used to predict the counterfactual outcome.
#' @param coef R Expression of leads and lags (eg `coef = -5:8`). By default estimates whole dynamic effect. See Details.
#' @param data A data frame or data.table
#' @param cohort Character. Time of treatment identifier. By default it expect `Inf` for never treated units. You can override this with `nevertreated.value`
#' @param w Numerical vector or Variable name. Default is `NULL`. Sample weights.
#' @param OATT Logical, default is `FALSE`. If `TRUE` then the overall average treatment effect is computed instead of ATT at each horizon.
#' @param unit Character, default is `NULL`. Unit identifier. If `NULL`, it default to the first fixed effect.
#' @param time Character, default is `NULL`. Time period identifier. If `NULL`, it default to the second fixed effect.
#' @param td Logical or Character, default is `FALSE`. Triple difference estimation. If `TRUE`, the third fixed effect in `y0` will be interpreted as the additional group. If Character, then the corresponding column will be interpreted as the new group.
#' @param nevertreated.value Any, default is `Inf`. Value encoding the cohort of never treated units.
#' @param effective.sample Numeric, default is `30`. Effective sample size under which the function will throw a warning. See details.
#' @param with.se Logical, default is `TRUE`. Should standard errors be reported
#' @param tol Numeric, default is `1e-6`. Tolerance level for weights convergence.
#' @param maxit Numeric, default is `100`. Maximum number of iterations for computing weights
#' @param mutatedata Logical, default is `FALSE`. USE AT YOUR OWN RISK. This option will modify your data in place instead
#'                   of making a copy of it which could be useful with large dataset if your RAM is limited.
#'                   It may result in lost observations and bugs.
#' @param verbose Numeric, default is `0`. Level of verbosity.
#'
#'
#' @details
#'
#' See below for additional details on some arguments.
#'
#' @section Estimated coefficients:
#'
#' By default, the function will estimate all coefficients available. You can
#' customize this behavior with the `coef` option. The option takes an R
#' expression of the form `-{leads}:{lags}`. The default behavior is to estimate
#' all available coefficients such that `coef = -Inf:Inf`.
#'
#' For pre-trend coefficients, by default the function will set the greatest
#' leads as the reference group if `-Inf` is set.
#'
#' @section Effective sample a.k.a. Herfindahl condition:
#'
#' Borusyak, K., Jaravel, X., & Spiess, J. give a condition on weights such that
#' consistency of estimators holds. This condition states that the Herfindahl
#' index of weights must converge to zero. Another interpretation is that the
#' inverse of the index is a measure of 'effective sample size'. Authors
#' recommand an effective sample size of at least 30. If the effective sample
#' size is lower, a warning will be thrown.
#'
#' @section Cohort:
#'
#' The `cohort` argument is the date of treatment of the observation. By default
#' it expect `Inf` for never treated individuals. You can override this behavior
#' by setting another value to `nevertreated.value`.
#'
#' @section Standard-errors:
#'
#' The standard errors are computed using an alternating projection method. You
#' can tweek its meta-parameter by changing `tol` and `maxit`.
#'
#' @return A didImputation object with the results of the imputation estimation.
#' \item{data}{Data used for estimation.}
#' \item{convergence_time}{User and CPU time for weights convergence.}
#' \item{pvalue}{p-value for the positive horizon estimates.}
#' \item{coeftable}{Table of regression coefficients.}
#' \item{wald}{Wald statistic of the pre-trend regression.}
#' \item{coefs}{Average treatment effects from the imputation procedure.}
#' \item{niterations}{Number of iterations it took to compute the weights.}
#' \item{pre_trends}{`fixest` regression object for the pre-trends estimation.}
#'
#'
#'
#'
#'
#' @author Antoine Mayerowitz
#' @author Maxime Gravoueille
#'
#' @importFrom rlang enexpr
#'
#' @references
#' Borusyak, K., Jaravel, X., & Spiess, J. (2021). Revisiting event study designs: Robust and efficient estimation. Working paper.
#'
#' Stata implementation from the authors:
#'
#' did_imputation (\url{https://github.com/borusyak/did_imputation})
#'
#' Another R implementation using sparse matrix inversion:
#' {didimputation} (\url{https://github.com/kylebutts/didimputation})
#'
#' @examples
#' #Load example data
#' data(did_simulated)
#'
#' # Estimate the overall average treatment effect on treated and all available pre-trends
#' didImputation(y ~ 0 | i + t,
#'               cohort = 'g',
#'               OATT = TRUE,
#'               data = did_simulated)
#'
#' # Estimate the full dynamic model
#' didImputation(y ~ 0 | i + t,
#'               cohort = 'g',
#'               data = did_simulated)
#'
#' # Estimate positive (lags) coefficients
#' didImputation(y ~ 0 | i + t,
#'               cohort = 'g',
#'               coef = 0:Inf,
#'               data = did_simulated)
#'
#' # Estimate first 3 post treatment coefficients
#' didImputation(y ~ 0 | i + t,
#'               cohort = 'g',
#'               coef = 0:2,
#'               data = did_simulated)
#'
#' # Return only point estimates
#' didImputation(y ~ 0 | i + t,
#'               cohort = 'g',
#'               with.se = FALSE,
#'               data = did_simulated)
#'
#'
#' @export
didImputation <- function(y0,
                          data,
                          cohort,
                          nevertreated.value = Inf,
                          unit = NULL,
                          time = NULL,
                          td = FALSE,
                          w = NULL,
                          coef = -Inf:Inf,
                          OATT = FALSE,
                          with.se = TRUE,
                          tol = 1e-6,
                          maxit = 100,
                          mutatedata = FALSE,
                          verbose = 0,
                          effective.sample = 30) {

  . <- NULL
  tau <- NULL
  k <- NULL

  # Configuration
  s <- list(
    time = time,
    cohort = cohort,
    unit = unit,
    w = w,
    y0 = y0,
    y = y0[[2]],
    td = td,
    fes = parseFEs(y0),
    coef = enexpr(coef),
    maxit = maxit,
    with.se = with.se,
    verbose = verbose,
    OATT = OATT,
    tol = tol,
    effective.sample = effective.sample,
    ncontrasts = 1,
    nevertreated.value = nevertreated.value
  )

  s$controls <- parseControls(s$y0)

  # Load data
  s$data <- if (mutatedata) data else copy(subsetData(data, s))

  # Coef level
  if(s$td != FALSE){
    if(s$td == TRUE) {
      s$td <- parseFEs(s$y0)[3]
    }
    s$by <- c('k', s$td)
    s$ncontrasts <- nlevels(factor(s$data[[s$td]]))
  } else {
    s$by <- 'k'
  }

  class(s) <- "didImputation"

  # Prepare the data used for estimation
  s <- prepare(s)

  # Impute counterfactual outcome
  s <- runImputation(s)

  # Compute ATT by horizon and get labels
  s$coefs <- s$data[d == 1, .(mean(tau)), by = eval(s$by)][k >= s$coef[[1]] & k <= s$coef[[2]]]
  if(s$ncontrasts == 1){
    coeflabs <- paste0("k::", s$coefs$k)
  } else {
    coeflabs <- paste0("k::", s$coefs$k, ":", s$td, "::", s$coefs[[s$td]])
  }
  s$coefs <- s$coefs$V1
  names(s$coefs) <- coeflabs

  if (s$with.se) {
    # Weights
    s <- computeWeights(s)

    s <- SE(s)

    s$t <- s$coefs / unlist(s$se)

    s$pvalue <- sapply(s$t, function(t) 2 * stats::pnorm(-abs(t)))
  }

  s$coeftable <- coeftable(s)

  # Pre-trends
  if (any(s$coef < -1)) {
    s <- preTrend(s)
    pre_table <- as.data.frame(fixest::coeftable(s$pre_trends),
      check.names = FALSE
    )
    s$coeftable <- rbind(pre_table, s$coeftable)
  }

  return(s)
}


coeftable <- function(s) {
  if (s$with.se) {
    data.frame(
      Estimate = s$coefs,
      "Std. Error" = unlist(s$se),
      `t value` = s$t,
      "Pr(>|t|))" = s$pvalue,
      check.names = FALSE
    )
  } else {
    na_vec <- rep(NA, length(s$coefs))

    data.frame(
      Estimate = s$coefs,
      "Std. Error" = na_vec,
      `t value` = na_vec,
      "Pr(>|t|))" = na_vec,
      check.names = FALSE
    )
  }
}
