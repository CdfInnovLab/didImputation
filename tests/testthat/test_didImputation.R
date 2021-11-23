data("did_simulated")
base_stagg <- fixest::base_stagg
dt_het <- didImputation::generateDidData(i = 1000,
                       t = 5,
                       treatment = ~ d * (ig+1) * (k+1)
)

basicReg <- rlang::expr(didImputation(
  y0 = y ~ 0 | i + t,
  cohort = "g",
  data = did_simulated
))

hetReg <- rlang::expr(didImputation(
  y0 = y ~ 0 | i + t,
  cohort = "g",
  het = "ig",
  data = dt_het
))

warnreg <- rlang::expr(didImputation(
  y0 = y ~ 0 | id + year,
  unit = "id",
  time = "year",
  cohort = "year_treated",
  nevertreated.value = 10000,
  data = base_stagg
))

test_that("Regression runs without errors", {
  expect_error(eval(basicReg), NA)
})

test_that("Estimation can include controls", {
  dt <- didImputation::generateDidData(i = 500,
                                          t = 5,
                                          control = TRUE,
                                          treatment = ~ d * (ig+1) * (k+1)
  )

  expect_error(didImputation(
    y0 = y ~ x1 | i + t,
    cohort = "g",
    data = dt
  ),
  NA)
})

test_that("Regression on simulated data gives the right results", {
  res <- eval(basicReg)

  error <- sum(res$coefs - c(0.973520, 2.085544, 2.990625, 3.980677, 4.774795)) < 1e-06

  expect_true(error)
})

test_that("Weights are correctly estimated and can predict point estimates", {
  res <- eval(basicReg)

  expect_true(t(res$data$.w_0) %*% res$data$y - res$coefs[1] < 1e-4)
})

test_that("Warn about small effective sample", {
  expect_warning(eval(warnreg), regexp = "Insufficient effective sample size")
})

test_that("Estimate overall ATT", {
  reg <- rlang::duplicate(basicReg)
  reg$OATT <- TRUE

  res <- eval(reg)

  expect_length(res$coefs, 1)
  expect_equal(res$coefs[1], 2.327853,
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
})


test_that("Heterogeneity regression runs without errors", {
  est <- didImputation(y ~ 0 | i + t,
      data = dt_het,
      het = "ig",
      cohort = "g")

  expect_length(est$coefs, 8)
})

test_that("User can set leads and lags", {
  reg <- rlang::duplicate(basicReg)
  reg$coef <- rlang::expr(-3:2)
  res <- eval(reg)

  # Custom leads and lags
  expect_error(res, NA)
  expect_length(res$coefs, 3)
  expect_length(res$pre_trends$coefficients, 3)

  # Automatic pre-trends
  reg$coef <- rlang::expr(-Inf:0)
  res <- eval(reg)
  expect_error(res, NA)
  expect_length(res$coefs, 1)
  expect_length(res$pre_trends$coefficients, 4)

  # Default behavior
  reg$coef <- rlang::expr(-Inf:Inf)
  res <- eval(reg)
  expect_error(res, NA)
  expect_length(res$coefs, 5)
  expect_length(res$pre_trends$coefficients, 4)
})

test_that("No name collision with internal variable", {
  did_simulated$s <- did_simulated$y
  did_simulated$k <-  did_simulated$i
  did_simulated$wei <- did_simulated$t

  collision_Reg <- rlang::expr(didImputation(
    y0 = s ~ 0 | k + wei,
    cohort = "g",
    data = did_simulated
  ))

  expect_error(eval(collision_Reg), NA)
})

test_that("User can provide weights", {
  dt_het$weight <- dt_het$ig + 0.000001

  res_weighted <- didImputation(
    y0 = y ~ 0 | i + t,
    cohort = "g",
    coef = 0:0,
    OATT = T,
    w = "weight",
    data = dt_het
  )

  res_unweighted <- didImputation(
    y0 = y ~ 0 | i + t,
    cohort = "g",
    OATT = T,
    data = dt_het[dt_het$ig == 1, ]
  )

  expect_equal(coef(res_weighted), coef(res_unweighted), tolerance = 1e-3)

})

test_that("Verbose run without error", {
  expect_output(didImputation(
    y0 = y ~ 0 | i + t,
    cohort = "g",
    data = did_simulated,
    verbose = 3
  ),
  regexp = "*convergence*")
})

test_that("Throw warning for no convergence", {
  expect_warning(didImputation(
    y0 = y ~ 0 | i + t,
    cohort = "g",
    OATT = TRUE,
    data = did_simulated,
    maxit = 2
  ), regexp = "*Max number of iterations*")
})

test_that("Throw error if user asks for more coefs than available", {
  expect_error(didImputation(
    y0 = y ~ 0 | i + t,
    cohort = "g",
    coef = -Inf:200,
    data = did_simulated,
  ), regexp = "*Lags must be lower*")

  expect_error(didImputation(
    y0 = y ~ 0 | i + t,
    cohort = "g",
    coef = -200:Inf,
    data = did_simulated,
  ), regexp = "*Leads must be greater*")
})

test_that("Estimation is robust to NA", {
  dt_gen <- generateDidData(i = 1000,
                            t = 5,
                            treatment = ~ d * (ig+1) * (k+1)

  )

  dt_gen[!is.na(dt_gen)][sample(seq(dt_gen[!is.na(dt_gen)]),length(dt_gen[!is.na(dt_gen)])*(0.10))] <- NA

  dt_gen <- as.data.frame(sapply(dt_gen, as.numeric))

  res <- function(){suppressWarnings(didImputation(y ~ 0 | i + t,
                       cohort = "g",
                       data = dt_gen, "warnings"))}

  # There should not be any NA standard error.
  expect_false(any(is.na(res()$se)))
  expect_message(res(), regexp = "*Removed*")

})

# Check methods

test_that("Coefs can be extracted", {
  reg <- eval(basicReg)

  expect_error(coef(reg), NA)
  expect_length(coef(reg), 5)
  expect_named(coef(reg))
  expect_type(reg$coeftable, "list")
  expect_length(reg$coeftable, 4)

  # table without SE
  reg <- didImputation(
    y0 = y ~ 0 | i + t,
    cohort = "g",
    data = did_simulated,
    with.se = FALSE
  )
  expect_error(coef(reg), NA)
})

test_that("ATT can be extracted", {
  reg <- rlang::duplicate(basicReg)
  reg$OATT <- TRUE
  reg <- coef(eval(reg))

  expect_length(reg, 1)
  expect_named(reg, "k::0")
})

test_that("Can print output", {
  res <- eval(basicReg)
  summary_res <- summary(res)

  expect_error(invisible(print(res)), NA)
  expect_error(invisible(summary(res)), NA)
  expect_output(print(res), regexp = "Event Study")
  expect_output(print(summary_res), regexp = "Event Study")
  expect_error(invisible(print(summary(res))), NA)
})

test_that("Can plot results", {
  res <- eval(basicReg)

  expect_error(invisible(plot(res)), NA)
  expect_error(invisible(didplot(res)), NA)
  expect_error(invisible(didplot(res, type = "IR")), NA)
  expect_error(invisible(didplot(res, type = "lol")), regexp = "*type*")
  expect_error(invisible(didplot(res, ci = -1)), regexp = "*Confidence*")
  expect_error(invisible(didplot(iris)), regexp = "*argument*")

  res_het <- eval(hetReg)
  expect_error(invisible(didplot(res)), NA)
  expect_error(invisible(didplot(res, type = "IR")), NA)
})


test_that("methods execute without errors", {
  res <- eval(basicReg)

  expect_error(invisible(print(res)), NA)
  expect_error(invisible(plot(res)), NA)
})

