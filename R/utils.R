#' Parse Fixed Effects
#'
#' Extract fixed effects from formula
#'
#' @param f Formula.
#' @param ... additional arguments.
#'
#' @return a character vector with fixed effects.
#'
parseFEs <- function(f, ...) {
  gsub(
    "[[:space:]]", "",
    strsplit(
      deparse(f[[3]][[3]]),
      "\\+"
    )[[1]]
  )
}


#' Subset data according to function aarguments
#'
#' @param data A data frame.
#' @param s A list containing model parameters.
#'
#' @return A data frame.
#'
subsetData <- function(data, s){
  vars_to_keep <- c(all.vars(s$y0), s$time, s$cohort, s$unit)
  if( is.character(s$w) ) {
    vars_to_keep <- c(vars_to_keep, s$w)
  }
  vars_to_keep <- unique(vars_to_keep)

  return(subset(data, select = vars_to_keep))
}


#' Parse control variables
#'
#' @param f Formula.
#' @param ... additional arguments.
#'
#' @importFrom rlang parse_exprs
#'
#' @return a vector of expressions.
#'
parseControls <- function(f, ...) {
  parse_exprs(
    grep("^[^\\d].*",
      gsub(
        "[[:space:]]", "",
        strsplit(
          deparse(f[[3]][[2]]),
          "\\+"
        )[[1]]
      ),
      perl = TRUE,
      value = TRUE
    )
  )
}


#' Generate longitudinal data with staggered treatment
#'
#' @param i Numeric. Number of units
#' @param t Numeric. Number of periods
#' @param hdfe Logical. Should high dimensional fixed effect be added?
#' @param control Logical. Should continuous control be added?
#' @param attrition.rate Numerical. Default is 0. Probability of missing observation.
#' @param treatment one sided formula, default is  ~ d * (k + 1). See details.
#'
#'
#' @section Treatment effect formula:
#'
#' By default the treatment effect is `~ d * (k + 1)` where `d` is an internal
#' dummy equal to one if observation is treated and `k` is the relative time to
#' event. User can define its own formula to create complex treatment and test
#' the model effectiveness.
#'
#' User has access to the following hidden variables:
#' - `h`: a cohort-wise randomly generated number
#' - `g`: Cohort identifier
#' - `a`: Individual fixed effect
#' - `b`: Time fixed effect
#' - `t`: Calendar time
#'
#' @section Default outcome:
#'
#' The default data generating process is:
#'
#' `y ~ a + b + hdfe + 2*x1 + d * (k + 1) + e`
#'
#' Variable description is available in previous section.
#'
#' @return a data.table with columns
#' \item{i}{Unit identifier.}
#' \item{t}{Calendar time.}
#' \item{g}{Time of treatment.}
#' \item{a}{Individual fixed effect. ~N(0,1)}
#' \item{b}{Time fixed effect. ~N(0,1)}
#' \item{x1}{A continuous, normally distributed control. ~N(0,2)}
#' \item{hdfe}{A random effect with \{i\}/50 modalities. ~ N(0,1)}
#' \item{e}{Residual. ~ N(0,2)}
#' \item{hdfefactor}{A fixed effect of size \{i\}/50.}
#' \item{k}{Relative time to treatment.}
#' \item{d}{Dummy variable defined as k >= 0}
#' \item{h}{Randomly generated variable at the level of the cohort.}
#' \item{y}{Outcome.}
#' \item{true_effect}{True average treatment effect on the treated.}
#'
#' @importFrom stats rnorm runif
#'
#' @export
#'
#' @examples
#' dt <- generateDidData(100,10)
#' head(dt)
#'
generateDidData <- function(i,
                            t,
                            hdfe = TRUE,
                            control = TRUE,
                            attrition.rate = 0,
                            treatment = ~ d * (k + 1)
                            ){

  N <- i*t

  dt <- data.frame(i = rep(1:i, each = t),
                   t = rep(1:t, i),
                   g = rep(sample(c(2:(t - 1), Inf), i, replace = TRUE), each = t),
                   a = rep(rnorm(i), each = t),
                   b = rep(rnorm(t), i),
                   x1 = rnorm(N, sd = 2),
                   hdfe = sample(rnorm(i/50), N, replace = T),
                   e = rnorm(N, sd = 2))

  # Naming hdfe
  dt$hdfefactor <- as.factor(dt$hdfe)
  levels(dt$hdfefactor) <- 1:length(levels(dt$hdfefactor))

  # Time to event
  dt$k <- dt$t - dt$g

  # Treated dummy
  dt$d <- as.numeric(dt$k >= 0)

  # Heterogeneous effects
  dt$h <- rnorm(1, sd = 0.1) * (dt$g - min(dt$g))
  dt$h[!is.finite(dt$h)] <- 0

  # Outcome
  dt$k[!is.finite(dt$k)] <- 0

  dt$y <- dt$e + dt$a + dt$b

  #compute the true causal effect
  dt$trueffect <- eval(treatment[[2]], envir = dt)

  dt$y <- dt$y + dt$trueffect

  if (control) {
    dt$y <- dt$y + 2 * dt$x1
  }

  if (hdfe) {
    dt$y <- dt$y + dt$hdfe
  }

  dt$k <- dt$t - dt$g

  if (attrition.rate > 0) {
    dt <- dt[runif(N) >= attrition.rate,]
  }

  return(dt)
}


