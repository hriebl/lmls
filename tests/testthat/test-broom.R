set.seed(1337)

n <- 100
x1 <- runif(n)
x2 <- runif(n)
x3 <- runif(n)
y <- rnorm(n, 0 + 1 * x1 + 1 * x3, exp(-3 + 1 * x2 + 1 * x3))
m <- lmls(y ~ x1 + x3, ~ x2 + x3)

# glance() --------------------------------------------------------------------

test_that("glance() returns right structure", {
  expect_s3_class(glance(m), "data.frame")

  expect_setequal(
    names(glance(m)),
    c("df", "logLik", "AIC", "BIC", "deviance", "df.residual")
  )
})

# tidy() ----------------------------------------------------------------------

test_that("tidy() returns right structure", {
  expect_s3_class(tidy(m), "data.frame")
  expect_equal(nrow(tidy(m)), 6)

  expect_setequal(
    names(tidy(m)),
    c("predictor", "term", "estimate", "std.error", "statistic", "p.value")
  )
})
