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
  .k <- NULL
  .d <- NULL
  . <- NULL
  # Check if data table
  s$data <- if (!is.data.table(s$data)) setDT(s$data) else s$data


  # Normalize weights
  if (!is.null(s$w)) {
    if (s$w %in% names(s$data)) {
      s$w <- s$data[[s$w]]
    }
    if(any(s$w <= 0)) {
      stop("Weights must be positive.")
    }
    s$data$.wei <- (s$w / collapse::fsum(s$w)) * fsum(s$w > 0)
  } else {
    s$data$.wei <- 1
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
  s$data$.k <- s$data[[s$time]] - s$data[[s$cohort]]

  if (s$OATT) {
    s$data[.k >= 0, .k := 0]
  }

  # create dummy for treatment status (0 := not-(yet)-treated)
  s$data[, .d := fifelse(.k >= 0, 1L, 0L)]

  setkeyv(s$data, c(s$fes, ".d"))

  s$coef <- parseCoef(s$coef, s$OATT, s$data$.k)


  # Compute the amount of weights to compute
  s$nweights <- if (!s$OATT) s$coef[2] + 1 else 1

  if(s$ncontrasts == 1){
    s$weights_cols <- paste0(".w_", (1:s$nweights - 1))
  } else {
    weight_comb <- unique(s$data[.k >= 0 & .k <= eval(s$coef[2])][is.finite(.k),
                                                               .(.k, eval(as.name(s$het)))])
    s$weights_cols <- paste0(".w_", weight_comb[[1]], "_", weight_comb[[2]])
  }

  return(s)
}

#' Parse coef argument expression
#'
#' @param coef Expression. The amount of leads and lags allowing for Inf.
#' @param OATT Logical. Should the
#' @param k Numeric vector. Relative time to event
#'
#' @importFrom collapse fmin fmax
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

  mink <- fmin(fk)
  pre <- ifelse(is.finite(pre), pre,  max(mink+1, -5))

  if(post > fmax(fk)) stop("coef[2] value is impossible. Lags must be lower or equal to ", fmax(fk))
  if(pre < (mink)) stop("coef[1] is impossible. Leads must be greater or equal to ", mink)

  return(c(pre, post))
}
