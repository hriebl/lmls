suppressPackageStartupMessages(library(mgcv))

set.seed(1337)

n <- 100
x1 <- runif(n)
x2 <- runif(n)
x3 <- runif(n)
y <- rnorm(n, 0 + 1 * x1 + 1 * x3, exp(-3 + 1 * x2 + 1 * x3))
m <- lmls(y ~ x1 + x3, ~ x2 + x3, light = FALSE)

mref <- gam(list(y ~ x1 + x3, ~ x2 + x3), family = gaulss(b = 0))

dim_or_length <- function(x) {
  if (!is.null(dim(x))) dim(x) else length(x)
}

test_simple_method <- function(fun, dim) {
  x <- fun(m)
  expect_type(x, "list")
  expect_equal(dim_or_length(x$location), dim)
  expect_type(x$location, "double")

  expect_equal(dim_or_length(x$scale), dim)
  expect_type(x$scale, "double")

  x <- fun(m, predictor = "location")
  expect_equal(dim_or_length(x), dim)
  expect_type(x, "double")

  x <- fun(m, predictor = "scale")
  expect_equal(dim_or_length(x), dim)
  expect_type(x, "double")
}

test_that("simple methods work", {
  test_simple_method(coef, 3)
  test_simple_method(vcov, c(3, 3))
  test_simple_method(fitted, n)
})

test_that("deviance() works", {
  expect_equal(deviance(m), deviance(mref))
})

test_that("logLik() works", {
  x <- logLik(m)
  expect_equal(attr(x, "nobs"), n)
  attr(x, "nobs") <- NULL

  expect_equal(x, logLik(mref))
})

test_that("predict() works", {
  test_simple_method(predict, n)

  x <- predict(m, data.frame(x1 = 1, x2 = 1, x3 = 1))
  expect_equal(x$location, sum(coef(m, "location")), ignore_attr = TRUE)
  expect_equal(x$scale, sum(coef(m, "scale")), ignore_attr = TRUE)
})

test_that("print() works", {
  expect_snapshot(print(m))
})

test_that("simulate() works", {
  seed1 <- get(".Random.seed", envir = .GlobalEnv)

  x <- simulate(m)
  expect_s3_class(x, "data.frame")
  expect_equal(dim(x), c(n, 1))

  y <- simulate(m)
  expect_true(any(x != y))

  seed2 <- get(".Random.seed", envir = .GlobalEnv)

  expect_true(any(seed1 != seed2))

  seed1 <- get(".Random.seed", envir = .GlobalEnv)

  x <- simulate(m, seed = 1337)
  y <- simulate(m, seed = 1337)
  expect_equal(x, y)

  seed2 <- get(".Random.seed", envir = .GlobalEnv)

  expect_equal(seed1, seed2)
})

test_that("summary() works", {
  expect_snapshot(summary(m))

  expect_error(summary(m, type = "boot"), "run boot\\(\\) first")
  expect_error(summary(m, type = "mcmc"), "run mcmc\\(\\) first")

  set.seed(1337)

  expect_snapshot(summary(boot(m), type = "boot"))
  expect_snapshot(summary(mcmc(m), type = "mcmc"))
})

test_that("resid() works", {
  expect_almost_equal <- function(object, expected) {
    expect_equal(object, expected, ignore_attr = TRUE, tolerance = 0.0001)
  }

  expect_almost_equal(resid(m), resid(mref))
  expect_almost_equal(resid(m, "response"), resid(mref, "response"))
  expect_almost_equal(resid(m, "pearson"), resid(mref, "pearson"))
})
