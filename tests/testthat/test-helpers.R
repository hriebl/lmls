set.seed(1337)

n <- 100
x1 <- runif(n)
x2 <- runif(n)
x3 <- runif(n)
y <- rnorm(n, 0 + 1 * x1 + 1 * x3, exp(-3 + 1 * x2 + 1 * x3))
m <- lslm(y ~ x1 + x3, ~ x2 + x3, light = FALSE)

beta <- gamma <- c(0, 0, 0)
m <- set_coef(m, "location", beta)
m <- set_coef(m, "scale", gamma)

test_that("setting coefficients works", {
  names(beta) <- c("(Intercept)", "x1", "x3")
  names(gamma) <- c("(Intercept)", "x2", "x3")

  expect_equal(coef(m, "location"), beta)
  expect_equal(coef(m, "scale"), gamma)
})

test_that("fitted values are updated", {
  expect_equal(unname(fitted(m, "location")), rep(0, n))
  expect_equal(unname(fitted(m, "scale")), rep(1, n))
})

test_that("residuals are updated", {
  expect_equal(unname(residuals(m)), y)
})

test_that("coefficient matrix works", {
  cm <- coefmat(m, "location")

  expect_true(is.matrix(cm))
  expect_equal(dim(cm), c(3, 4))
  expect_equal(rownames(cm), c("(Intercept)", "x1", "x3"))
  expect_equal(colnames(cm), c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
})

test_that("coefficient matrix from samples works", {
  cm <- coefmat_samples(mcmc(m), "location", "mcmc")

  expect_true(is.matrix(cm))
  expect_equal(dim(cm), c(3, 4))
  expect_equal(rownames(cm), c("(Intercept)", "x1", "x3"))
  expect_equal(colnames(cm), c("Mean", "2.5%", "50%", "97.5%"))
})
