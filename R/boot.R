#' Parametric bootstrap for LMLS
#'
#' A simple parametric bootstrap algorithm for location-scale regression models
#' from the [lmls()] function.
#'
#' @param m A location-scale regression model from the [lmls()] function.
#' @param num_samples The number of bootstrap samples to draw.
#' @param seed Either `NULL` or an integer that will be used in a call to
#'             [set.seed()] before simulating the response vectors.
#'
#' @return
#'
#' An `lmls` S3 object, see [lmls()]. The entry `boot` with the matrices of
#' bootstrap samples is added to the object as a list with the names `location`
#' and `scale`.
#'
#' @examples
#' library(lmls)
#' m <- lmls(y ~ poly(x, 2), ~ x, data = abdom, light = FALSE)
#' m <- boot(m)
#' summary(m, type = "boot")
#' hist(m$boot$scale[, 2])
#' @importFrom stats coef simulate
#' @export

boot <- function(m, num_samples = 1000, seed = NULL) {
  if (m$light) {
    stop("Cannot bootstrap, lmls() called with argument 'light = TRUE'")
  }

  sims <- simulate(m, num_samples, seed)

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
