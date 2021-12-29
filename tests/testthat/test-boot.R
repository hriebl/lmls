set.seed(1337)

n <- 100
x1 <- runif(n)
x2 <- runif(n)
x3 <- runif(n)
y <- rnorm(n, 0 + 1 * x1 + 1 * x3, exp(-3 + 1 * x2 + 1 * x3))
m <- lmls(y ~ x1 + x3, ~ x2 + x3, light = FALSE)
m <- boot(m)

# error with light model ------------------------------------------------------

test_that("boot() throws error with light model", {
  m <- lmls(y ~ x1 + x3, ~ x2 + x3)
  expect_error(boot(m), "light")
})

# attributes of the bootstrap samples -----------------------------------------

test_that("bootstrap samples have the right dimension", {
  expect_equal(nrow(m$boot$location), 1000)
  expect_equal(nrow(m$boot$scale), 1000)

  expect_equal(ncol(m$boot$location), 3)
  expect_equal(ncol(m$boot$scale), 3)

  m <- boot(m, num_samples = 500)
  expect_equal(nrow(m$boot$location), 500)
  expect_equal(nrow(m$boot$scale), 500)
})

test_that("bootstrap samples have the right colnames", {
  expect_equal(colnames(m$boot$location), c("(Intercept)", "x1", "x3"))
  expect_equal(colnames(m$boot$scale), c("(Intercept)", "x2", "x3"))
})

# bootstrap means and variances-covariances -----------------------------------

test_that("bootstrap means are correct", {
  expect_roughly(colMeans(m$boot$location), c(0, 1, 1))
  expect_roughly(colMeans(m$boot$scale), c(-3, 1, 1))
})

test_that("bootstrap variances-covariances are correct", {
  expect_roughly(cov(m$boot$location), vcov(m, "location"))
  expect_roughly(cov(m$boot$scale), vcov(m, "scale"))
})
