set.seed(1337)

n <- 100
x1 <- runif(n)
x2 <- runif(n)
x3 <- runif(n)
y <- rnorm(n, 0 + 1 * x1 + 1 * x3, exp(-3 + 1 * x2 + 1 * x3))
m <- lmls(y ~ x1 + x3, ~ x2 + x3, light = FALSE)
m <- mcmc(m)

# error with light model ------------------------------------------------------

test_that("mcmc() throws error with light model", {
  m <- lmls(y ~ x1 + x3, ~ x2 + x3)
  expect_error(mcmc(m), "light")
})

# attributes of the MCMC samples ----------------------------------------------

test_that("MCMC samples have the right dimension", {
  expect_equal(nrow(m$mcmc$location), 1000)
  expect_equal(nrow(m$mcmc$scale), 1000)

  expect_equal(ncol(m$mcmc$location), 3)
  expect_equal(ncol(m$mcmc$scale), 3)

  m <- mcmc(m, num_samples = 500)
  expect_equal(nrow(m$mcmc$location), 500)
  expect_equal(nrow(m$mcmc$scale), 500)
})

test_that("MCMC samples have the right colnames", {
  expect_equal(colnames(m$mcmc$location), c("(Intercept)", "x1", "x3"))
  expect_equal(colnames(m$mcmc$scale), c("(Intercept)", "x2", "x3"))
})

# posterior means and variances-covariances -----------------------------------

test_that("posterior means are correct", {
  expect_roughly(colMeans(m$mcmc$location), c(0, 1, 1))
  expect_roughly(colMeans(m$mcmc$scale), c(-3, 1, 1))
})

test_that("posterior variances-covariances are correct", {
  expect_roughly(cov(m$mcmc$location), vcov(m, "location"))
  expect_roughly(cov(m$mcmc$scale), vcov(m, "scale"))
})

# Geweke's convergence diagnostic ---------------------------------------------

test_that("MCMC chains converged according to Geweke", {
  gwk <- coda::geweke.diag(m$mcmc$location)
  expect_true(all(pnorm(gwk$z) > 0.025))
  expect_true(all(pnorm(gwk$z) < 0.975))

  gwk <- coda::geweke.diag(m$mcmc$scale)
  expect_true(all(pnorm(gwk$z) > 0.025))
  expect_true(all(pnorm(gwk$z) < 0.975))
})

# dual averaging --------------------------------------------------------------

test_that("target acceptance rate decreases step size", {
  m1 <- dual_averaging(m, target_accept = 0.8)
  step1 <- m1$step_size

  m2 <- dual_averaging(m, target_accept = 0.9)
  step2 <- m2$step_size

  m3 <- dual_averaging(m, target_accept = 0.99)
  step3 <- m3$step_size

  expect_gt(step1, step2)
  expect_gt(step2, step3)
})

# update functions ------------------------------------------------------------

test_that("MMALA update returns right structure", {
  up <- mmala_update(m, "location", 1)

  expect_type(up, "list")
  expect_s3_class(up$m, "lmls")
  expect_type(up$alpha, "double")

  up <- mmala_update(m, "scale", 1)

  expect_type(up, "list")
  expect_s3_class(up$m, "lmls")
  expect_type(up$alpha, "double")
})

test_that("Gibbs update returns right structure", {
  m <- gibbs_update_beta(m)
  expect_s3_class(m, "lmls")
})
