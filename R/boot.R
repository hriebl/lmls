#' Parametric bootstrap for location-scale regression
#'
#' A simple parametric bootstrap algorithm for location-scale regression models
#' from the [lmls()] function.
#'
#' @param m A location-scale regression model from the [lmls()] function.
#' @param nsim The number of bootstrap samples to draw.
#' @param seed Either `NULL` or an integer that will be used in a call to
#'             [set.seed()] before simulating the response vectors.
#'
#' @importFrom stats coef simulate
#' @export

boot <- function(m, nsim = 1000, seed = NULL) {
  if (m$light) {
    stop("Cannot bootstrap, lmls() called with argument 'light = TRUE'")
  }

  sims <- simulate(m, nsim, seed)

  coefs <- lapply(sims, function(y) {
    coef(lmls(y ~ 0 + m$x, ~ 0 + m$z))
  })

  samples_beta <- lapply(coefs, `[[`, "location")
  samples_beta <- do.call(rbind, samples_beta)
  colnames(samples_beta) <- colnames(m$x)

  samples_gamma <- lapply(coefs, `[[`, "scale")
  samples_gamma <- do.call(rbind, samples_gamma)
  colnames(samples_gamma) <- colnames(m$z)

  m$boot <- list(location = samples_beta, scale = samples_gamma)

  m
}
