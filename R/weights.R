#----------------------------------------------#
# Author: Antoine Mayerowitz and Maxime Gravoueille
# Date creation: Mon May 24 15:17:00 2021
# ~: Compute weights
#----------------------------------------------#

#' Compute weights
#'
#' @param s a "didImputation" model object
#' @param ... other arguments.
#'
#' @importFrom collapse fmean fsum
#' @importFrom purrr discard
#' @importFrom fixest demean
#'
#' @return a didImputation extended model
#'
computeWeights <- function(s, ...) {
  # Must declare NSE variables to avoid CRAN Check error
  wei <- NULL
  k <- NULL
  w1 <- NULL
  guess <- NULL

  dt <- s$data

  dt[d == 1, w1 := 1 / .N, by = k]

  dt[, s$weights_cols := 0]

  # Compute the denominator, which is the fraction of
  # untreated obs in each dimensions
  # However, if I include that in the iteration
  # it does not converge....
  computeDenomFe <- function(fe) {
    dt[d == 0, paste0("denom_", fe) := sum(wei), by = c(fe)]
  }

  computeDenomControl <- function(c, cname) {
    cmean <- eval(expr(dt[d == 0, fmean(!!c, w = wei)]))

    eval(expr(dt[, paste0("dm_", cname) := !!c - cmean])) # demeaned control
    # return the denominator, it is a scalar
    return(
      dt[d == 0, fsum(wei * get(paste0("dm_", cname))^2)]
    )
  }

  # Initialize the weights
  initweight <- function(w_i, .i) {
    w1 <- NULL
    dt[k == .i, paste0(w_i) := w1]
  }


  # compute denominator for fixed effects
  for (fe in s$fes) {
    computeDenomFe(fe)
  }

  # Compute denominator for controls
  for (c in s$controls) {
    cname <- deparse(c)
    s$denom_c[[cname]] <- computeDenomControl(c, cname)
  }

  # Initialize weights
  for (i in 1:s$nweights) {
    initweight(paste0("w_", i - 1), i - 1)
  }

  # Check if effective sample size is large enough
  if (s$effective.sample > 0) {
    smallSample <- discard(s$weights_cols, hasEffectiveSample, dt, 1 / s$effective.sample)

    if (length(smallSample) != 0) {
      warning(paste0(
        paste0(smallSample, collapse = " ,"),
        ": Insufficient effective sample size for those horizons. Keep in mind that the corresponding estimates may be unreliable and SE may be downward biased."
      ))
    }
  }

  # Smart guess, may be smarter in the futur
  for (w in s$weights_cols) {
    dt[, guess := demean(dt[[w]], dt[, .SD, .SDcols = s$fes ])]
    dt[d == 0, paste0(w) := guess]
  }

  # Compute weights
  start_time <- proc.time()
  for (w in s$weights_cols) {
    if (s$verbose > 1) cat("Estimating ", w, "\n")
    s$niterations[[w]] <- iterateWeight(s, dt, 1, w)
  }
  end_time <- proc.time()

  s$convergence_time <- (end_time - start_time)

  if (s$verbose > 1) {
    cat("Weights computation time\n", s$convergence_time, "\n")
  }

  return(s)
}

hasEffectiveSample <- function(wi, dt, hhi) {
  wei <- NULL
  dt[d == 1, fsum(abs(get(wi))^2, w = wei)] <= hhi
}


#' Recursively estimate weights until convergence
#'
#' @param params List. Object of class DiDImputation.
#' @param dt data.table. Internal regression table.
#' @param i Integer. Recursion index.
#' @param w Character. Name of the weight to estimate.
#'
#' @return Integer. The recursion index (i.e, the number of function calls)
#'
iterateWeight <- function(params, dt, i, w) {
  old_w <- copy(dt[[w]])

  # Compute weights wrt fixed effects
  for (fe in params$fes) {
    computeWeightFe(dt, w, paste0("denom_", fe), fe)
  }

  # Compute weights wrt controls
  for (c in params$controls) {
    cname <- deparse(c)
    computeWeightControl(dt, w, params$denom_c[[cname]], paste0("dm_", cname))
  }

  # Compute fit statistic
  fit <- sum(abs(dt[[w]] - old_w))

  if (params$verbose > 1) cat(i, " ", fit, "\n")

  # Recursion base case.
  # Stops the function if fit < tol | i > maxit.
  if (fit > params$tol & i < params$maxit) {
    iterateWeight(params, dt, i + 1, w)
  } else if (i > params$maxit) {
    warning(paste0(
      "Weights convergence: Max number of iterations reached for ",
      w,
      ". Try increasing the number of iterations or reduce the tolerance."
    ))
    return(i)
  } else {
    if (params$verbose > 1) cat(w, "reached convergence in ", i, "iterations\n")
    return(i)
  }
}

#' Single Weight Iteration
#'
#' Single iteration of weight demean w.r.t. fixed effect.
#'
#' @param dt data.table.
#' @param w_i Character. Weight to demean.
#' @param denom Numeric. Relative size of untreated population.
#' @param fe Character. Name of the fixed effect to demean on.
#'
#' @return
#' Void. Mutate the weight vector in the original data.table.
#'
#' @importFrom  collapse fsum
#'
computeWeightFe <- function(dt, w_i, denom, fe) {
  # w_i is equal to 1/.N for d==1 & k == i
  # Create event study weights
  sumw <- NULL
  wei <- NULL
  dt[, sumw := fsum(get(w_i), get(fe), wei, TRA = "replace_fill")]
  dt[d == 0, (w_i) := get(w_i) - sumw / get(denom)]
}

#' One iteration of weight
#'
#' Single iteration of weight demean w.r.t. continuous control.
#'
#' @param dt data.table.
#' @param w_i Character. Weight to demean.
#' @param denom Numeric. Relative size of untreated population.
#' @param c Character. Name of the control to demean on.
#'
#' @return
#' Void. Mutate the weight vector in the original data.table.
#'
#' @importFrom collapse fsum
#'
computeWeightControl <- function(dt, w_i, denom, c) {
  # w_i is equal to 1/.N for d==1 & k == i
  # Create event study weights
  sumw <- NULL
  wei <- NULL

  dt[, sumw := fsum(get(w_i) * get(c), wei, TRA = "replace_fill")]
  dt[d == 0, (w_i) := get(w_i) - sumw * get(c) / denom]
}
