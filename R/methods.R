#' coef method for object of class "didImputation"
#'
#' @inheritParams stats::coef
#'
#' @export coef.didImputation
#' @export
coef.didImputation <- function(object, ...) {
  return(object$coefs)
}

#' DiD imputation summary result
#'
#' @inheritParams base::summary
#'
#' @importFrom stats symnum
#'
#' @export summary.didImputation
#' @export
summary.didImputation <- function(object, ...) {
  s <- object

  ans <- list(
    dep = s$y,
    call = s$y0,
    Ncohorts = length(unique(s$data[[s$cohort]]))-1,
    residuals = sqrt(s$data$wei) * s$data$eps,
    N = nrow(s$data),
    N0 = nrow(s$data[d == 0]),
    N1 = nrow(s$data[d == 1]),
    weights = s$data$wei,
    rss = sum(s$data$wei * s$data$eps^2),
    horizons = s$coef,
    coeftable = s$coeftable,
    weights_cols = s$data[, .SD, .SDcols = s$weights_cols],
    signif = symnum(s$coeftable$`Pr(>|t|))`, corr = FALSE, na = FALSE,
                     cutpoints = c(0, 0.01, 0.05, 0.1, 1),
                     symbols = c("***", "**", "*", ""))
  )

  if (s$coef[1] < 0) {
    ans$pre_trend <- s$pre_trends
    ans$pre_wald <- s$wald
  }

  class(ans) <- "summary.didImputation"

  return(ans)
}

#' Print method for summary of class "didImputation"
#'
#' @param x A list of class "summary.didImputation"
#' @param digits Decimal precision in output.
#' @param ... additional arguments.
#'
#' @return An estimation table
#' @export print.summary.didImputation
#' @export
print.summary.didImputation <- function(x, digits = 3, ...){
  ans <- x
  dt <- round(ans$coeftable, digits)

  p_threshold <- 1*10^(-digits)
  p_threshold_str <- formatC(p_threshold, digits)

  dt$`Pr(>|t|))` <- ifelse(dt$`Pr(>|t|))` < p_threshold,
                           paste0('<',p_threshold_str),
                           dt$`Pr(>|t|))`)

  dt$`Pr(>|t|))` <- paste0(dt$`Pr(>|t|))`, ans$signif)


  cat("Event Study: imputation method. Dep. Var.: ", ans$dep, "\n")
  cat("Counterfactual model: ", deparse(ans$call), "\n")
  cat("Number of cohorts: ", ans$Ncohorts, "\n")
  cat("Observations:", ans$N, "\n")
  cat("|-Treated:   ", ans$N1, "\n")
  cat("|-Untreated: ", ans$N0, "\n")
  print(dt, right = FALSE)
  cat("---\n")
  cat("Signif. Code:", attr(ans$signif, "legend"), "\n")

  if (any(ans$horizons < 0)) {
    cat("Wald stats for pre-trends:\n")
    print(ans$pre_wald)
  }
}

#' Print didImputation result
#'
#' @param x an object of class "didImputation".
#' @param ... additional arguments.
#'
#' @importFrom utils tail
#'
#' @export print.didImputation
#' @export
print.didImputation <- function(x, ...) {
  cat("Event Study: imputation method. Dep. Var.: ", x$y, "\n")
  cat("Counterfactual model: ", deparse(x$y0), "\n")
  cat("Observations:", nrow(x$data), "\n")
  print(tail(x$coeftable, n = 5L))
  if (nrow(x$coeftable > 5)) {
    cat(" ......", nrow(x$coeftable) - 5, " rows not shown.\n")
  }
}


#' DiD imputation plot
#'
#' @description
#' Takes a didImputation estimation and return a graph of estimated
#' parameters(for dynamic effects only).
#'
#' @param object an object of class DidImputation.
#' @param type Character. Type of plot. Can be either 'default' or 'IR' for
#' impulse response style.
#' @param ci Numeric. Confidence interval( 0.95 by default).
#' @param ... additional arguments.
#'
#' @return A ggplot2 object
#'
#' @import ggplot2
#'
#' @importFrom stats qnorm
#'
#' @examples
#' # standard plot
#' res <- didImputation(y ~ 0 | i + t,
#'               cohort = 'g',
#'               data = did_simulated)
#'
#' didplot(res)
#'
#' # Plot with 99% confidence interval
#'
#' didplot(res, ci = 0.99)
#'
#' # Plot with Impulse response function style
#'
#' didplot(res, type = 'IR')
#'
#'
#' @export
didplot <- function(object,
                    type = "default",
                    ci = 0.95,
                    ...) {
  with(object, {

    # Extract timing (and group in case of triple difference)
    if(object$ncontrast == 1) {
      coeftable$x <- as.numeric(
        sapply(
          strsplit(
            rownames(coeftable), "::"
          ), function(x) x[2]
        )
      )
    } else {
      coeftable$x <- as.numeric(
        sapply(
          strsplit(
            rownames(coeftable), ":"
          ), function(x) x[3]
        )
      )
      coeftable$group <- as.factor(
        sapply(
          strsplit(
            rownames(coeftable), ":"
          ), function(x) x[6]
        )
      )
    }

    q <- qnorm(ci + (1 - ci) / 2)

    ylab <- paste0("Estimate and ", ci * 100, "% Conf. Int.")

    if (!any(coeftable$x == -1, na.rm = TRUE) & min(coeftable$x, na.rm = TRUE) < -1) {
      ref <- data.frame(
        Estimate = 0,
        row.names = paste0("k::", -1),
        x = -1,
        "Std. Error" = NaN,
        "t value" = NaN,
        "Pr(>|t|))" = NaN,
        check.names = FALSE
      )

      coeftable <- rbind(coeftable, ref)
    }


    if(ncontrasts == 1) {
      p <- ggplot(coeftable, aes(x = x, y = Estimate))
    } else {
      p <- ggplot(coeftable, aes(x = x, y = Estimate, color = group))
    }

    p <- p +
      xlab("Time to treatment") +
      ylab(ylab) +
      scale_x_continuous(breaks = seq(min(coeftable$x, na.rm = T), max(coeftable$x, na.rm = T))) +
      theme_classic() +
      theme(
        panel.grid.major.y = element_line(colour = "lightgrey", linetype = "dotted"),
        axis.text = element_text(size = 12)
      )

    if (any(coeftable$x < 0)) p <- p + geom_vline(xintercept = -0.5, color = "firebrick")


    if (type == "default") {
      p <- p +
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_point() +
        geom_errorbar(aes(
          ymin = Estimate - `Std. Error` * q,
          ymax = Estimate + `Std. Error` * q
        ),
        width = 0.2
        )
    } else if (type == "IR") {
      p <- p +
        geom_hline(yintercept = 0, linetype = "solid") +
        geom_line() +
        if(ncontrasts == 1){
          geom_ribbon(aes(
            ymax = Estimate + `Std. Error` * q,
            ymin = Estimate - `Std. Error` * q,
          ), linetype = 2, alpha = 0, colour = "black")
        } else {
          geom_ribbon(aes(
            ymax = Estimate + `Std. Error` * q,
            ymin = Estimate - `Std. Error` * q,
          ), linetype = 2, alpha = 0)
        }
    }

    return(p)
  })
}

#' DiD imputation plot
#'
#' @inherit didplot
#'
#' @export plot.didImputation
#' @export
#'
plot.didImputation <- function(...) {
  didplot(...)
}
