#' @importFrom generics tidy
#' @export

generics::tidy

#' @importFrom generics tidy
#' @importFrom stats coef pnorm vcov
#' @export

tidy.lmls <- function(x, predictor = c("location", "scale"), ...) {
  predictor <- match.arg(predictor, several.ok = TRUE)

  if (length(predictor) > 1) {
    out <- lapply(predictor, function(p) tidy(x, p))
    out <- do.call(rbind, out)
  } else {
    std.error <- sqrt(diag(vcov(x, predictor)))
    statistic <- coef(x, predictor) / std.error
    p.value <- 2 * pnorm(-abs(statistic))

    out <- data.frame(
      predictor = predictor,
      term      = names(coef(x, predictor)),
      estimate  = coef(x, predictor),
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

#' @importFrom stats AIC BIC deviance df.residual
#' @export

glance.lmls <- function(x, ...) {
  data.frame(
    df          = x$df,
    logLik      = loglik(x),
    AIC         = AIC(x),
    BIC         = BIC(x),
    deviance    = deviance(x),
    df.residual = df.residual(x)
  )
}
