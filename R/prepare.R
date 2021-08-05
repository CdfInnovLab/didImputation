#----------------------------------------------#
# Author: Antoine Mayerowitz and Maxime Gravoueille
# Date creation: Mon May 24 15:17:00 2021
# ~: Prepare the data for imputation
#----------------------------------------------#

#' Create variables for treatment status and horizon.
#'
#' @param s A list with the arguments to didImputation.
#'
#' @import data.table
#' @importFrom collapse fmin fmax fsum
#' @importFrom stats na.omit
#' @return Augmented list with configured arguments.
#'
prepare <- function(s) {
  k <- NULL
  # Check if data table
  s$data <- if (!is.data.table(s$data)) setDT(s$data) else s$data


  # Normalize weights
  if (!is.null(s$w)) {
    if (s$w %in% names(s$data)) {
      s$w <- s$data[[s$w]]
    }
    s$data$wei <- (s$w / collapse::fsum(s$w)) * length(na.omit(s$w))
  } else {
    s$data$wei <- 1
  }

  if (is.null(s$unit)) {
    s$unit <- s$fes[1]
  }

  if (is.null(s$time)) {
    s$time <- s$fes[2]
  }

  if (s$nevertreated.value != Inf) {
    s$data[get(s$cohort) == (s$nevertreated.value), (s$cohort) := Inf]
  }

  # create time to event variable
  # Only pre and post if OATT is true
  s$data[, k := get(s$time) - get(s$cohort)]

  if (s$OATT) {
    s$data[k >= 0, k := 0]
  }

  # create dummy for treatment status (0 := not-(yet)-treated)
  s$data[, d := fifelse(k >= 0, 1L, 0L)]

  setkeyv(s$data, c(s$fes, "d"))

  s$coef <- parseCoef(s$coef, s$OATT, s$data$k)


  s$nweights <- if (!s$OATT) s$coef[2] + 1 else 1
  s$weights_cols <-
    sapply(1:s$nweights, function(x) {
      paste0("w_", x - 1)
    })

  return(s)
}

#' Parse coef argument expression
#'
#' @param coef Expression. The amount of leads and lags allowing for Inf.
#' @param OATT Logical. Should the
#' @param k Numeric vector. Relative time to event
#'
#' @return Numeric vector of length 2.
#'
parseCoef <- function(coef, OATT, k){
  fk <- k[is.finite(k)]
  pre <- eval(coef[[2]])
  post <- eval(coef[[3]])

  if (OATT) {
    post <- 0
  } else {
    post <- ifelse(is.finite(post), post,  fmax(fk))
  }

  pre <- ifelse(is.finite(pre), pre,  fmin(fk) + 1)

  return(c(pre, post))
}
