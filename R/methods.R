#' Methods for LMLS
#'
#' A couple of methods for location-scale regression models from the [lmls()]
#' function.
#'
#' @param object A location-scale regression model from the [lmls()] function.
#' @param predictor The predictor to work on. Either `"location"` or `"scale"`
#'                  or both. If both, a list with the names `"location"` and
#'                  `"scale"` is returned.
#' @param ... Currently ignored.
#' @param newdata A data frame (or list or environment) with the covariate
#'                values at which the predictions are computed. If `NULL`, the
#'                predictions at the original data are returned.
#' @param type Used by `predict()` and `residuals()`:
#'             \itemize{
#'               \item For `predict()`, `"link"` or `"response"`. If `"link"`
#'                     (default), \eqn{\mu} and log(\eqn{\sigma}) are returned.
#'                     If `"response"`, \eqn{\mu} and \eqn{\sigma}
#'                     are returned.
#'               \item For `residuals()`, `"deviance"`, `"pearson"` or
#'                     `"response"`. If `"deviance"` (default) or `"pearson"`,
#'                     (\eqn{y - \mu}) / \eqn{\sigma} is returned.
#'                     If `"response"`, \eqn{y - \mu} is returned.
#'             }
#'
#' @name lmls-methods

NULL

#' @rdname lmls-methods
#' @export

coef.lmls <- function(object, predictor = c("location", "scale"), ...) {
  predictor <- match.arg(predictor, several.ok = TRUE)

  if (length(predictor) == 1) {
    object$coefficients[[predictor]]
  } else {
    object$coefficients[predictor]
  }
}

#' @importFrom stats resid
#' @export

deviance.lmls <- function(object, ...) {
  sum(resid(object, "deviance")^2)
}

#' @rdname lmls-methods
#' @export

fitted.lmls <- function(object, predictor = c("location", "scale"), ...) {
  predictor <- match.arg(predictor, several.ok = TRUE)

  if (length(predictor) == 1) {
    object$fitted.values[[predictor]]
  } else {
    object$fitted.values[predictor]
  }
}

#' @importFrom stats nobs
#' @export

logLik.lmls <- function(object, ...) {
  out <- loglik(object)

  attr(out, "df") <- object$df
  attr(out, "nobs") <- nobs(object)
  class(out) <- "logLik"

  out
}

#' @importFrom graphics abline plot
#' @importFrom stats fitted qnorm resid
#' @export

plot.lmls <- function(x,
                      xlab = "Fitted values",
                      ylab = "Deviance residuals",
                      ...) {
  plot(
    x = fitted(x, "location"),
    y = resid(x, "deviance"),
    xlab = xlab,
    ylab = ylab,
    ...
  )

  abline(h = qnorm(c(0.025, 0.975)), lty = "dashed")
  abline(h = 0)

  invisible(x)
}

#' @rdname lmls-methods
#' @importFrom stats as.formula coef fitted model.matrix predict update
#' @export

predict.lmls <- function(object,
                         newdata = NULL,
                         predictor = c("location", "scale"),
                         type = c("link", "response"),
                         ...) {
  predictor <- match.arg(predictor, several.ok = TRUE)
  type <- match.arg(type)

  if (length(predictor) > 1) {
    out <- lapply(predictor, function(p) {
      predict(object, newdata, p, type)
    })

    names(out) <- predictor

    return(out)
  }

  if (is.null(newdata)) {
    out <- fitted(object, predictor)

    if (predictor == "scale" && type == "link") {
      out <- log(out)
    }

    return(out)
  }

  arg <- object$call[[predictor]]
  formula <- update(as.formula(arg), NULL ~ .)
  x <- model.matrix(formula, newdata)

  beta <- coef(object, predictor)
  out <- drop(x %*% beta)

  if (predictor == "scale" && type == "response") {
    out <- exp(out)
  }

  out
}

#' @importFrom stats coef
#' @export

print.lmls <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat(
    "\nCall:\n",
    paste(deparse(x$call), sep = "\n", collapse = "\n"),
    "\n\n",
    sep = ""
  )

  pretty <- function(x) {
    print.default(format(x, digits = digits), print.gap = 2, quote = FALSE)
  }

  if (length(coef(x, "location"))) {
    cat("Location coefficients (identity link):\n")
    pretty(coef(x, "location"))
  } else {
    cat("No location coefficients\n")
  }

  cat("\n")

  if (length(coef(x, "scale"))) {
    cat("Scale coefficients (log link):\n")
    pretty(coef(x, "scale"))
  } else {
    cat("No scale coefficients\n")
  }

  cat("\n")

  invisible(x)
}

#' @importFrom stats qqline qqnorm resid
#' @export

qqnorm.lmls <- function(y,
                        xlab = "Theoretical quantiles",
                        ylab = "Deviance residuals",
                        ...) {
  qqnorm(resid(y, "deviance"), xlab = xlab, ylab = ylab, ...)
  qqline(resid(y, "deviance"))

  invisible(y)
}

#' @rdname lmls-methods
#' @importFrom stats fitted
#' @export

residuals.lmls <- function(object,
                           type = c("deviance", "pearson", "response"),
                           ...) {
  type <- match.arg(type)
  out <- object$residuals

  if (type != "response") {
    out <- out / fitted(object, "scale")
  }

  out
}

#' @importFrom stats fitted nobs rnorm runif
#' @export

simulate.lmls <- function(object, nsim = 1, seed = NULL, ...) {
  # RNG handling taken from simulate.lm():
  # https://github.com/wch/r-source/blob/master/src/library/stats/R/lm.R

  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    runif(1)  # initialize the RNG if necessary
  }

  if (is.null(seed)) {
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  } else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }

  n <- nobs(object) * nsim
  out <- rnorm(n, fitted(object, "location"), fitted(object, "scale"))
  out <- matrix(out, nrow = nobs(object), ncol = nsim)

  out <- as.data.frame(out)
  names(out) <- paste("sim", seq_len(nsim), sep = "_")
  attr(out, "seed") <- RNGstate

  out
}

#' Summary for LMLS
#'
#' Prints a summary for location-scale regression models from the [lmls()]
#' function.
#'
#' @param object A location-scale regression model from the [lmls()] function.
#' @param type Either `"ml"` or `"boot"` or `"mcmc"`:
#'             \itemize{
#'               \item If `"ml"`, the maximum likelihood estimates and the
#'                     asymptotic standard errors are shown.
#'               \item If `"boot"`, the bootstrap estimates and confidence
#'                     intervals are shown.
#'               \item If `"mcmc"`, the Markov chain Monte Carlo (MCMC)
#'                     estimates and credible intervals are shown.
#'             }
#' @param digits The number of digits to print.
#' @param ... Passed on to [printCoefmat()].
#'
#' @importFrom stats AIC BIC coef df.residual printCoefmat resid
#' @export

summary.lmls <- function(object,
                         type = c("ml", "boot", "mcmc"),
                         digits = max(3, getOption("digits") - 3),
                         ...) {
  type <- match.arg(type)

  if (type != "ml") {
    coefmat <- function(m, predictor) coefmat_samples(m, predictor, type)

    if (is.null(object[[type]]$location) || is.null(object[[type]]$scale)) {
      stop("Model does not include samples, run ", type, "() first")
    }
  }

  cat(
    "\nCall:\n",
    paste(deparse(object$call), sep = "\n", collapse = "\n"),
    "\n\n",
    sep = ""
  )

  if (df.residual(object) > 5) {
    cat("Deviance residuals:\n")
    print(summary(resid(object, "deviance"), digits = digits))
    cat("\n")
  }

  if (length(coef(object, "location"))) {
    cat("Location coefficients (identity link):\n")
    printCoefmat(coefmat(object, "location"), digits = digits, ...)
  } else {
    cat("No location coefficients\n")
  }

  cat("\n")

  if (length(coef(object, "scale"))) {
    cat("Scale coefficients (log link):\n")
    printCoefmat(coefmat(object, "scale"), digits = digits, ...)
  } else {
    cat("No scale coefficients\n")
  }

  cat("\n")

  pretty <- function(x, y) {
    cat(x, ": ", format(signif(y), getOption("digits")), "\n", sep = "")
  }

  pretty("Residual degrees of freedom", df.residual(object))
  pretty("Log-likelihood", loglik(object))
  pretty("AIC", AIC(object))
  pretty("BIC", BIC(object))

  cat("\n")

  invisible(object)
}

#' @rdname lmls-methods
#' @export

vcov.lmls <- function(object, predictor = c("location", "scale"), ...) {
  predictor <- match.arg(predictor, several.ok = TRUE)

  if (length(predictor) == 1) {
    object$vcov[[predictor]]
  } else {
    object$vcov[predictor]
  }
}
