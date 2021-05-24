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
