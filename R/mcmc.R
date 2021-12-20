#' @importFrom stats coef fitted lm.wfit

gibbs_update_beta <- function(curr_m) {
  fit <- lm.wfit(curr_m$x, curr_m$y, fitted(curr_m, "scale")^(-2))

  mu <- coef(fit)
  chol_sig_inv <- chol(info_beta(curr_m))
  next_beta <- rmvnorm(1, mu, chol_sig_inv)
  next_m <- set_beta(curr_m, next_beta)

  next_m
}

#' @importFrom stats coef

mmala_propose <- function(curr_m, predictor, step_size) {
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

  mu <- curr_coef + step_size^2 / 2 * step
  chol_sig_inv <- curr_chol_info / step_size
  prop_coef <- drop(rmvnorm(1, mu, chol_sig_inv))
  forward <- dmvnorm(prop_coef, mu, chol_sig_inv, log = TRUE)
  list(prop_coef = prop_coef, forward = forward)
}

#' @importFrom stats coef

mmala_backward <- function(curr_m, prop_m, predictor, step_size) {
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

  mu <- prop_coef + step_size^2 / 2 * step
  chol_sig_inv <- prop_chol_info / step_size
  dmvnorm(curr_coef, mu, chol_sig_inv, log = TRUE)
}

#' @importFrom stats runif

mmala_update <- function(curr_m, predictor, step_size) {
  proposal <- mmala_propose(curr_m, predictor, step_size)
  prop_coef <- proposal$prop_coef
  forward <- proposal$forward

  prop_m <- set_coef(curr_m, predictor, prop_coef)
  backward <- mmala_backward(curr_m, prop_m, predictor, step_size)

  alpha <- loglik(prop_m) - loglik(curr_m) + backward - forward

  if (log(runif(1)) <= alpha) {
    list(m = prop_m, alpha = alpha)
  } else {
    list(m = curr_m, alpha = alpha)
  }
}

# see https://colindcarroll.com/2019/04/21/step-size-adaptation-in-hamiltonian-monte-carlo/
# and https://mc-stan.org/docs/2_28/reference-manual/hmc-algorithm-parameters.html

dual_averaging <- function(m, num_warmup = 1000, target_accept = 0.8,
                           gamma = 0.05, kappa = 0.75, t0 = 10) {
  log_step <- 0
  log_avg_step <- 0
  error_sum <- 0

  for (i in 1:num_warmup) {
    m <- gibbs_update_beta(m)
    m <- mmala_update(m, "scale", exp(log_step))
    alpha <- m$alpha
    m <- m$m

    accept_prob <- min(exp(alpha), 1)
    error_sum <- error_sum + target_accept - accept_prob
    log_step <- -error_sum / (gamma * sqrt(t0 + i))

    eta <- (t0 + i)^(-kappa)
    log_avg_step <- (1 - eta) * log_avg_step + eta * log_step
  }

  list(m = m, step_size = exp(log_avg_step))
}

#' MCMC inference for location-scale regression
#'
#' @description
#'
#' A Markov chain Monte Carlo (MCMC) sampler for the use with location-scale
#' regression models from the [lmls()] function. The sampler uses Gibbs updates
#' for the location coefficients and the Riemann manifold Metropolis-adjusted
#' Langevin algorithm (MMALA) from Girolami and Calderhead (2011) with the
#' Fisher-Rao metric tensor for the scale coefficients. The priors for the
#' regression coefficients are assumed to be flat.
#'
#' To find the optimal step size for the MMALA updates, the dual averaging
#' algorithm from Nesterov (2009) is used during a warm-up phase.
#'
#' @param m A location-scale regression model from the [lmls()] function.
#' @param num_samples The number of MCMC samples after the warm-up.
#'                    Defaults to 1000.
#' @param num_warmup The number of MCMC samples for the warm-up.
#'                   Defaults to 1000.
#' @param target_accept The target acceptance rate for the dual averaging
#'                      algorithm used for the warm-up. Defaults to 0.8.
#'
#' @references
#' Girolami, M. and Calderhead, B. (2011), Riemann manifold Langevin and
#' Hamiltonian Monte Carlo methods. Journal of the Royal Statistical Society:
#' Series B (Statistical Methodology), 73: 123-214.
#' <https://doi.org/10.1111/j.1467-9868.2010.00765.x>
#'
#' Nesterov, Y. (2009), Primal-dual subgradient methods for convex problems.
#' Mathematical Programming, 120: 221–259.
#' <https://doi.org/10.1007/s10107-007-0149-x>
#'
#' @importFrom stats coef
#' @export

mcmc <- function(m, num_samples = 1000, num_warmup = 1000,
                 target_accept = 0.8) {
  if (m$light) {
    stop("Cannot run MCMC, lmls() called with argument 'light = TRUE'")
  }

  m <- dual_averaging(m, num_warmup, target_accept)
  step_size <- m$step_size
  m <- m$m

  m$mcmc <- list(
    location = matrix(nrow = num_samples, ncol = ncol(m$x),
                      dimnames = list(NULL, colnames(m$x))),
    scale = matrix(nrow = num_samples, ncol = ncol(m$z),
                   dimnames = list(NULL, colnames(m$z)))
  )

  for (i in 1:num_samples) {
    m <- gibbs_update_beta(m)
    m <- mmala_update(m, "scale", step_size)
    m <- m$m

    m$mcmc$location[i,] <- coef(m, "location")
    m$mcmc$scale[i,] <- coef(m, "scale")
  }

  m
}
