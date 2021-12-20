#' @export

coef.lmls <- function(object, predictors = c("location", "scale"), ...) {
  predictors <- match.arg(predictors, several.ok = TRUE)

  if (length(predictors) > 1) {
    out <- object$coefficients[predictors]
  } else {
    out <- object$coefficients[[predictors]]
  }

  out
}

#' @importFrom stats resid
#' @export

deviance.lmls <- function(object, ...) {
  sum(resid(object, "pearson")^2)
}

#' @export

fitted.lmls <- function(object, predictors = c("location", "scale"), ...) {
  predictors <- match.arg(predictors, several.ok = TRUE)

  if (length(predictors) > 1) {
    out <- object$fitted.values[predictors]
  } else {
    out <- object$fitted.values[[predictors]]
  }

  out
}

#' @importFrom stats dnorm fitted nobs
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
                      ylab = "Pearson residuals",
                      ...) {
  plot(
    x = fitted(x, "location"),
    y = resid(x, "pearson"),
    xlab = xlab,
    ylab = ylab,
    ...
  )

  abline(h = qnorm(c(0.025, 0.975)), lty = "dashed")
  abline(h = 0)
  invisible(x)
}

#' @importFrom stats as.formula coef fitted model.matrix predict update
#' @export

predict.lmls <- function(object,
                         newdata = NULL,
                         type = c("link", "response"),
                         predictors = c("location", "scale"),
                         ...) {
  predictors <- match.arg(predictors, several.ok = TRUE)
  type <- match.arg(type)

  if (is.null(newdata)) {
    out <- fitted(object, predictors)

    if (type == "link" && any(predictors == "scale")) {
      out$scale <- log(out$scale)
    }
  } else {
    if (length(predictors) > 1) {
      out <- lapply(predictors, function(p) {
        predict(object, newdata, type, p)
      })

      names(out) <- predictors
    } else {
      formula <- as.formula(object$call[[predictors]])
      formula <- update(formula, NULL ~ .)

      mm <- model.matrix(formula, newdata)
      out <- drop(mm %*% coef(object, predictors))

      if (type == "response" && predictors == "scale") {
        out <- exp(out)
      }
    }
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

  if (length(coef(x, "location"))) {
    cat("Location coefficients:\n")

    print.default(
      x = format(coef(x, "location"), digits = digits),
      print.gap = 2,
      quote = FALSE
    )
  } else {
    cat("No location coefficients\n")
  }

  cat("\n")

  if (length(coef(x, "scale"))) {
    cat("Scale coefficients:\n")

    print.default(
      x = format(coef(x, "scale"), digits = digits),
      print.gap = 2,
      quote = FALSE
    )
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
                        ylab = "Pearson residuals",
                        ...) {
  qqnorm(resid(y, "pearson"), xlab = xlab, ylab = ylab, ...)
  qqline(resid(y, "pearson"))
  invisible(y)
}

#' @importFrom stats fitted
#' @export

residuals.lmls <- function(object, type = c("response", "pearson"), ...) {
  type <- match.arg(type)
  out <- object$residuals

  if (type == "pearson") {
    out <- out / fitted(object, "scale")
  }

  out
}

#' @importFrom stats fitted nobs rnorm runif
#' @export

simulate.lmls <- function(object, nsim = 1, seed = NULL, ...) {
  # RNG handling taken from simulate.lm:
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
  dim(out) <- c(nobs(object), nsim)

  out <- as.data.frame(out)
  names(out) <- paste("sim", seq_len(nsim), sep = "_")
  attr(out, "seed") <- RNGstate

  out
}

#' @importFrom stats AIC BIC coef df.residual printCoefmat resid
#' @export

summary.lmls <- function(object,
                         digits = max(3, getOption("digits") - 3),
                         type = c("ml", "boot", "mcmc"),
                         ...) {
  type <- match.arg(type)

  if (type != "ml") {
    coefmat <- function(m, predictor) {
      coefmat_samples(m, predictor, type)
    }

    if (is.null(object[[type]]$location) || is.null(object[[type]]$scale)) {
      stop("Model does not include samples, run boot() or mcmc() first")
    }
  }

  cat(
    "\nCall:\n",
    paste(deparse(object$call), sep = "\n", collapse = "\n"),
    "\n\n",
    sep = ""
  )

  if (df.residual(object) > 5) {
    cat("Pearson residuals:\n")
    print(summary(resid(object, "pearson"), digits = digits))
    cat("\n")
  }

  if (length(coef(object, "location"))) {
    cat("Location coefficients (identity link function):\n")
    printCoefmat(coefmat(object, "location"), digits = digits, ...)
  } else {
    cat("No location coefficients\n")
  }

  cat("\n")

  if (length(coef(object, "scale"))) {
    cat("Scale coefficients (log link function):\n")
    printCoefmat(coefmat(object, "scale"), digits = digits, ...)
  } else {
    cat("No scale coefficients\n")
  }

  cat("\n")

  cat(
    "Residual degrees of freedom:",
    format(signif(df.residual(object), digits)),
    "\n"
  )

  cat(
    "Log-likelihood:",
    format(signif(loglik(object), digits)),
    "\n"
  )

  cat(
    "AIC:",
    format(signif(AIC(object), digits)),
    "\n"
  )

  cat(
    "BIC:",
    format(signif(BIC(object), digits)),
    "\n\n"
  )

  invisible(object)
}

#' @export

vcov.lmls <- function(object, predictors = c("location", "scale"), ...) {
  predictors <- match.arg(predictors, several.ok = TRUE)

  if (length(predictors) > 1) {
    out <- object$vcov[predictors]
  } else {
    out <- object$vcov[[predictors]]
  }

  out
}
