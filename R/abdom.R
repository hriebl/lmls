#' Abdominal Circumference Data
#'
#' The `abdom` data frame has 610 rows and 2 columns. The data are
#' measurements of abdominal circumference (response variable) taken from
#' fetuses during ultrasound scans at Kings College Hospital, London, at
#' gestational ages (explanatory variable) ranging between 12 and 42 weeks.
#'
#' @usage
#'
#' data(abdom)
#'
#' @format
#'
#' This data frame contains the following columns:
#'
#' \describe{
#'   \item{y}{Abdominal circumference: a numeric vector.}
#'   \item{x}{Gestational age: a numeric vector.}
#' }
#'
#' @details
#'
#' The data were used to derive reference intervals by Chitty *et al.*
#' (1994) and also for comparing different reference centile methods by
#' Wright and Royston (1997), who also commented that the distribution of
#' z-scores obtained from the different fitted models "has somewhat longer
#' tails than the normal distribution".
#'
#' @source
#'
#' Dr. Eileen M. Wright, Department of Medical Statistics and Evaluation,
#' Royal Postgraduate Medical School, Du Cane Road, London, W12 0NN.
#'
#' The `abdom` dataset was copied into the `lmls` package from the
#' `gamlss.data` package. `gamlss.data` is licensed under the GPL 2 and 3.
#'
#' @references
#'
#' Chitty, L.S., Altman, D.G., Henderson, A. and Campbell, S. (1994). Charts
#' of fetal size: 3, abdominal measurement. *Br. J. Obstet. Gynaec.*,
#' **101**: 125–131.
#'
#' Wright, E.M. and Royston, P. (1997). A comparison of statistical
#' methods for age-related reference intervals. *J. R. Statist. Soc. A.*,
#' **160**: 47–69.
#'
#' @examples
#' data(abdom)
#' attach(abdom)
#' plot(x, y)
#' detach(abdom)
#' @name abdom

NULL
