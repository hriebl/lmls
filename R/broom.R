#' @importFrom generics tidy
#' @export

generics::tidy

#' @importFrom generics tidy
#' @importFrom stats coef pnorm vcov
#' @export

tidy.lmls <- function(x, predictors = c("location", "scale"), ...) {
  predictors <- match.arg(predictors, several.ok = TRUE)

  if (length(predictors) > 1) {
    out <- lapply(predictors, function(p) tidy(x, p))
    out <- do.call(rbind, out)
  } else {
    std.error <- sqrt(diag(vcov(x, predictors)))
    statistic <- coef(x, predictors) / std.error
    p.value <- 2 * pnorm(-abs(statistic))

    out <- data.frame(
      predictor = predictors,
      term      = names(coef(x, predictors)),
      estimate  = coef(x, predictors),
      std.error = std.error,
      statistic = statistic,
      p.value   = p.value,
      row.names = NULL
    )
  }

  out
}

#' @importFrom generics glance
#' @export

generics::glance

#' @importFrom stats AIC BIC deviance df.residual logLik
#' @export

glance.lmls <- function(x, ...) {
  data.frame(
    df          = x$df,
    logLik      = logLik(x),
    AIC         = AIC(x),
    BIC         = BIC(x),
    deviance    = deviance(x),
    df.residual = df.residual(x)
  )
}
