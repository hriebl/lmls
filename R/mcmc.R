#' @importFrom stats coef

propose <- function(curr_m, predictor, stepsize) {
  curr_coef <- coef(curr_m, predictor)
  curr_score <- score(curr_m, predictor)
  curr_chol_info <- chol_info(curr_m, predictor)

  step <- backsolve(
    r = curr_chol_info,
    x = forwardsolve(
      l = curr_chol_info,
      x = curr_score,
      upper.tri = TRUE,
      transpose = TRUE
    )
  )

  mu <- curr_coef + stepsize^2 / 2 * step
  chol_sig_inv <- curr_chol_info / stepsize
  prop_coef <- drop(rmvnorm(1, mu, chol_sig_inv))
  forward <- dmvnorm(prop_coef, mu, chol_sig_inv, log = TRUE)
  list(prop_coef = prop_coef, forward = forward)
}

#' @importFrom stats coef

backward <- function(curr_m, prop_m, predictor, stepsize) {
  curr_coef <- coef(curr_m, predictor)
  prop_coef <- coef(prop_m, predictor)
  prop_score <- score(prop_m, predictor)
  prop_chol_info <- chol_info(prop_m, predictor)

  step <- backsolve(
    r = prop_chol_info,
    x = forwardsolve(
      l = prop_chol_info,
      x = prop_score,
      upper.tri = TRUE,
      transpose = TRUE
    )
  )

  mu <- prop_coef + stepsize^2 / 2 * step
  chol_sig_inv <- prop_chol_info / stepsize
  dmvnorm(curr_coef, mu, chol_sig_inv, log = TRUE)
}

#' @importFrom stats logLik runif

mcmc_update <- function(curr_m, predictor, stepsize) {
  proposal <- propose(curr_m, predictor, stepsize)
  prop_coef <- proposal$prop_coef
  forward <- proposal$forward

  prop_m <- set_coef(curr_m, predictor, prop_coef)
  backward <- backward(curr_m, prop_m, predictor, stepsize)

  alpha <- logLik(prop_m) - logLik(curr_m) + backward - forward

  if (log(runif(1)) <= alpha) {
    prop_m
  } else {
    curr_m
  }
}

#' MCMC inference for location-scale regression
#'
#' A Markov chain Monte Carlo (MCMC) sampler for location-scale regression
#' models from the [lslm()] function. The sampler uses the Riemann manifold
#' Metropolis-adjusted Langevin algorithm (MMALA) from Girolami and Calderhead
#' (2011) with the Fisher-Rao metric tensor. All priors in the model are
#' assumed to be flat.
#'
#' @param m A location-scale regression model from the [lslm()] function.
#' @param nsim The number of MCMC samples to draw.
#' @param stepsize The step size of the MCMC update.
#'
#' @references
#' Girolami, M. and Calderhead, B. (2011), Riemann manifold Langevin and
#' Hamiltonian Monte Carlo methods. Journal of the Royal Statistical Society:
#' Series B (Statistical Methodology), 73: 123-214.
#' <https://doi.org/10.1111/j.1467-9868.2010.00765.x>
#'
#' @importFrom stats coef
#' @export

mcmc <- function(m, nsim = 1000, stepsize = sqrt(3) * (m$df)^(-1/6)) {
  if (m$light) {
    stop("Cannot run MCMC, lslm() called with argument 'light = TRUE'")
  }

  mcmc_m <- m

  samples_beta <- matrix(nrow = nsim, ncol = ncol(m$x))
  colnames(samples_beta) <- colnames(m$x)

  samples_gamma <- matrix(nrow = nsim, ncol = ncol(m$z))
  colnames(samples_gamma) <- colnames(m$z)

  for (i in 1:nsim) {
    mcmc_m <- mcmc_update(mcmc_m, "location", stepsize)
    mcmc_m <- mcmc_update(mcmc_m, "scale", stepsize)

    samples_beta[i,] <- coef(mcmc_m, "location")
    samples_gamma[i,] <- coef(mcmc_m, "scale")
  }

  m$mcmc <- list(location = samples_beta, scale = samples_gamma)

  m
}
