set.seed(1337)

n <- 100
x1 <- runif(n)
x2 <- runif(n)
x3 <- runif(n)
y <- rnorm(n, 0 + 1 * x1 + 1 * x3, exp(-3 + 1 * x2 + 1 * x3))

# integration tests -----------------------------------------------------------

m <- lslm(y ~ x1 + x3, ~ x2 + x3)

test_that("lslm() estimates are close to true values", {
  expect_roughly(coef(m, "location"), c(0, 1, 1))
  expect_roughly(coef(m, "scale"), c(-3, 1, 1))
})

# unit tests: setup() ---------------------------------------------------------

m <- setup(y ~ x1 + x3, ~ x2 + x3, light = FALSE)

test_that("setup() creates list with correct names", {
  n <- c("y", "x", "z", "nobs", "df", "df.residual", "light",
         "call", "coefficients", "fitted.values", "residuals",
         "vcov", "chol_info_gamma", "iterations")

  expect_true(all(n %in% names(m)))
})

test_that("setup() creates list with correct values", {
  expect_equal(m$y, y)

  expect_exactly(m$x, cbind(1, x1, x3))
  expect_exactly(m$z, cbind(1, x2, x3))

  expect_equal(m$nobs, 100)

  expect_equal(m$df, 6)
  expect_equal(m$df.residual, 94)

  expect_true(is.numeric(m$chol_info_gamma))
  expect_equal(dim(m$chol_info_gamma), c(3, 3))
})

test_that("setup() sets class attribute", {
  expect_s3_class(m, "lslm")
})

test_that("setup() argument 'data' works with data frame", {
  dat <- data.frame(y = y, x4 = x1, x5 = x2, x6 = x3)
  m <- setup(y ~ x4 + x6, ~ x5 + x6, data = dat)

  expect_error(m, NA)
})

test_that("setup() argument 'data' works with list", {
  dat <- list(y = y, x4 = x1, x5 = x2, x6 = x3)
  m <- setup(y ~ x4 + x6, ~ x5 + x6, data = dat)

  expect_error(m, NA)
})

# unit tests: init_beta(), init_gamma() ---------------------------------------

m <- init_beta(m)

test_that("init_beta() initializes elements with numeric", {
  expect_true(is.numeric(coef(m, "location")))
  expect_true(is.numeric(fitted(m, "location")))
  expect_true(is.numeric(resid(m)))
})

m <- init_gamma(m)

test_that("init_gamma() initializes elements with numeric", {
  expect_true(is.numeric(coef(m, "scale")))
  expect_true(is.numeric(fitted(m, "scale")))
})

# unit tests: update_beta(), update_gamma() -----------------------------------

test_that("update_beta() updates elements", {
  old_coef <- coef(m, "location")
  old_fitted <- fitted(m, "location")
  old_resid <- resid(m)

  m <- update_beta(m)

  expect_error(expect_equal(coef(m, "location"), old_coef))
  expect_error(expect_equal(fitted(m, "location"), old_fitted))
  expect_error(expect_equal(resid(m), old_resid))
})

test_that("update_gamma() updates elements", {
  old_coef <- coef(m, "scale")
  old_fitted <- fitted(m, "scale")

  m <- update_gamma(m)

  expect_error(expect_equal(coef(m, "scale"), old_coef))
  expect_error(expect_equal(fitted(m, "scale"), old_fitted))
})

# unit tests: estimate() ------------------------------------------------------

m <- estimate(m)

test_that("estimate() stores number of iterations", {
  expect_true(is.numeric(m$iterations))
})

# unit tests: finish() --------------------------------------------------------

m$light <- FALSE
m <- finish(m)

test_that("finish() sets coefficient covariance matrices", {
  expect_true(is.numeric(vcov(m, "location")))
  expect_equal(dim(vcov(m, "location")), c(3, 3))

  expect_true(is.numeric(vcov(m, "scale")))
  expect_equal(dim(vcov(m, "scale")), c(3, 3))
})

m$light <- TRUE
m <- finish(m)

test_that("finish() removes elements if 'light' is TRUE", {
  expect_false("x" %in% names(m))
  expect_false("z" %in% names(m))

  expect_false("chol_info_gamma" %in% names(m))
})
