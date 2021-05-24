data("did_simulated")
base_stagg <- fixest::base_stagg

basicReg <- rlang::expr(didImputation(
  y0 = y ~ 0 | i + t,
  cohort = "g",
  data = did_simulated
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

test_that("Regression on simulated data gives the right results", {
  res <- eval(basicReg)

  error <- sum(res$coefs - c(0.973520, 2.085544, 2.990625, 3.980677, 4.774795)) < 1e-05

  expect_true(error)
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


test_that("methods execute without errors", {
  res <- eval(basicReg)

  expect_error(invisible(print(res)), NA)
  expect_error(invisible(summary(res)), NA)
  expect_error(invisible(didplot(res)), NA)
})


