set.seed(1337)

d <- 10

mu <- runif(d)
sig <- matrix(runif(d^2), d)
sig <- crossprod(sig)

sig_inv <- chol2inv(chol(sig))
chol_sig_inv <- chol(sig_inv)

n <- 5000

x <- rmvnorm(n, mu, chol_sig_inv)

test_that("sampling from multivariate normal works", {
  expect_equal(rowMeans(x), mu, tolerance = 0.05)
  expect_equal(cov(t(x)), sig, tolerance = 0.05)
})

test_that("multivariate normal density works", {
  expect_equal(
    dmvnorm(x[,1], mu, chol_sig_inv, log = TRUE),
    mvtnorm::dmvnorm(x[,1], mu, sig, log = TRUE)
  )

  expect_equal(
    dmvnorm(x[,2], mu, chol_sig_inv, log = FALSE),
    mvtnorm::dmvnorm(x[,2], mu, sig, log = FALSE)
  )

  expect_equal(
    dmvnorm(x[,3], mu, chol_sig_inv),
    mvtnorm::dmvnorm(x[,3], mu, sig)
  )

  expect_equal(
    dmvnorm(x, mu, chol_sig_inv, log = TRUE),
    mvtnorm::dmvnorm(t(x), mu, sig, log = TRUE)
  )
})
