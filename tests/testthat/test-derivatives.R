library(numDeriv)

set.seed(1337)

n <- 100
x1 <- runif(n)
x2 <- runif(n)
x3 <- runif(n)
y <- rnorm(n, 0 + 1 * x1 + 1 * x3, exp(-3 + 1 * x2 + 1 * x3))
m <- lslm(y ~ x1 + x3, ~ x2 + x3, light = FALSE)

# score -----------------------------------------------------------------------

beta <- gamma <- c(0, 0, 0)
m <- set_coef(m, "location", beta)
m <- set_coef(m, "scale", gamma)

f <- function(x, predictor) {
   logLik(set_coef(m, predictor, x))
}

test_that("score of beta is correct", {
  num_score <- grad(f, beta, predictor = "location")
  expect_equal(score(m, "location"), num_score, ignore_attr = "names")
})

test_that("score of gamma is correct", {
  num_score <- grad(f, gamma, predictor = "scale")
  expect_equal(score(m, "scale"), num_score, ignore_attr = "names")
})

# fisher info -----------------------------------------------------------------

beta <- c(0, 1, 1)
gamma <- c(-3, 1, 1)
m <- set_coef(m, "location", beta)
m <- set_coef(m, "scale", gamma)

nsim <- 5000

reps <- replicate(nsim, {
  y <- rnorm(n, 0 + 1 * x1 + 1 * x3, exp(-3 + 1 * x2 + 1 * x3))
  m <- lslm(y ~ x1 + x3, ~ x2 + x3, light = FALSE)
  m <- set_coef(m, "location", beta)
  m <- set_coef(m, "scale", gamma)
  c(score_beta(m), score_gamma(m))
})

num_info <- cov(t(reps))

test_that("fisher info of beta is correct", {
  expect_equal(info_beta(m), num_info[1:3, 1:3], tolerance = 0.05)
})

test_that("fisher info of gamma is correct", {
  expect_equal(info_gamma(m), num_info[4:6, 4:6], tolerance = 0.05)
})

test_that("cholesky decomposition of fisher info works", {
  expect_equal(chol_info(m, "location"), chol(info_beta(m)))
  expect_equal(chol_info(m, "scale"), chol(info_gamma(m)))
})
