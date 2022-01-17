#' @importFrom stats coef fitted lm.fit resid

init_beta <- function(m) {
  fit <- lm.fit(m$x, m$y)

  m$coefficients$location <- coef(fit)
  m$fitted.values$location <- fitted(fit)
  m$residuals <- resid(fit)

  m
}

#' @importFrom stats coef fitted lm.fit resid

init_gamma <- function(m) {
  # -(log(2) + digamma(1 / 2)) / 2 = 0.6351814
  # see https://en.wikipedia.org/wiki/Unbiased_estimation_of_standard_deviation
  # and https://arxiv.org/abs/1503.06266

  fit <- lm.fit(m$z, 0.6351814 + log(abs(resid(m, "response"))))

  m$coefficients$scale <- coef(fit)
  m$fitted.values$scale <- exp(fitted(fit))

  m
}

#' @importFrom stats coef fitted lm.wfit resid

update_beta <- function(m) {
  fit <- lm.wfit(m$x, m$y, fitted(m, "scale")^(-2))

  m$coefficients$location <- coef(fit)
  m$fitted.values$location <- fitted(fit)
  m$residuals <- resid(fit)

  m
}

#' @importFrom stats coef

update_gamma <- function(m) {
  step <- backsolve(
    r = m$chol_info_gamma,
    x = forwardsolve(
      l = m$chol_info_gamma,
      x = score_gamma(m),
      upper.tri = TRUE,
      transpose = TRUE
    )
  )

  m <- set_gamma(m, coef(m, "scale") + step)

  m
}

#' @importFrom stats model.matrix update

setup <- function(location,
                  scale = ~1,
                  data = environment(location),
                  light = TRUE,
                  call = NULL) {
  scale <- update(scale, paste(location[[2]], "~."))
  y <- eval(location[[2]], data, environment(location))
  x <- model.matrix(location, data)
  z <- model.matrix(scale, data)

  nobs <- length(y)
  df <- ncol(x) + ncol(z)

  m <- structure(
    list(
      y               = y,
      x               = x,
      z               = z,
      nobs            = nobs,
      df              = df,
      df.residual     = nobs - df,
      light           = light,
      call            = call,
      coefficients    = list(location = NULL, scale = NULL),
      fitted.values   = list(location = NULL, scale = NULL),
      residuals       = NULL,
      vcov            = list(location = NULL, scale = NULL),
      chol_info_gamma = NULL,
      iterations      = NULL
    ),
    class = "lmls"
  )

  m$chol_info_gamma <- chol(info_gamma(m))

  m
}

estimate <- function(m, maxit = 100, reltol = sqrt(.Machine$double.eps)) {
  m <- init_beta(m)
  m <- init_gamma(m)
  m <- update_beta(m)

  it <- 0
  enough <- TRUE

  while (it < maxit && enough) {
    it <- it + 1

    before <- loglik(m)
    m <- update_gamma(m)
    m <- update_beta(m)
    after <- loglik(m)

    enough <- abs(after - before) > reltol * (abs(before) + reltol)
  }

  if (enough) {
    warning("Estimation did not converge, maximum number of iterations reached")
  }

  m$iterations <- it

  m
}

finish <- function(m) {
  m$vcov$location <- chol2inv(chol(info_beta(m)))
  m$vcov$scale <- chol2inv(m$chol_info_gamma)

  if (m$light) {
    m$x <- m$z <- m$chol_info_gamma <- NULL
  }

  m
}

#' Gaussian location-scale regression
#'
#' @description
#'
#' The location-scale regression model assumes a normally distributed response
#' variable with one linear predictor for the mean (= the location) and one for
#' the standard deviation (= the scale). The standard deviation is mapped to
#' the linear predictor through a log link.
#'
#' This function sets up the model object and estimates it with maximum
#' likelihood.
#'
#' @param location A two-sided formula with the response variable on the LHS
#'                 and the predictor for the mean on the RHS.
#' @param scale A one-sided formula with the predictor for the standard
#'              deviation on the RHS.
#' @param data A data frame (or list or environment) in which to evaluate
#'             the `location` and `scale` formulas.
#' @param light If `TRUE`, the design matrices are removed from the estimated
#'              model to save some memory.
#' @param maxit The maximum number of iterations of the Fisher scoring
#'              algorithm.
#' @param reltol The relative convergence tolerance of the Fisher scoring
#'               algorithm.
#'
#' @return
#'
#' A fitted linear model for location and scale as an `lmls` S3 object.
#' The object has at least the following entries:
#'
#' - `y`: the response vector
#' - `nobs`: the number of observations
#' - `df`: the degrees of freedom
#' - `df.residual`: the residual degrees of freedom
#' - `coefficients`: the regression coefficients as a list with the names
#'   `location` and `scale`
#' - `fitted.values`: the fitted values as a list with the names `location`
#'   and `scale`
#' - `residuals`: the response residuals
#' - `coefficients`: the variance-covariance matrices of the regression
#'   coefficients as a list with the names `location` and `scale`
#' - `iterations`: the number of iterations the Fisher scoring algorithm
#'   took to converge
#'
#' @examples
#' library(lmls)
#' m <- lmls(y ~ poly(x, 2), ~ x, data = abdom)
#' summary(m)
#' plot(m)
#' qqnorm(m)
#' @export

lmls <- function(location,
                 scale = ~1,
                 data = environment(location),
                 light = TRUE,
                 maxit = 100,
                 reltol = sqrt(.Machine$double.eps)) {
  m <- setup(location, scale, data, light, match.call())
  m <- estimate(m, maxit, reltol)
  m <- finish(m)
  m
}
