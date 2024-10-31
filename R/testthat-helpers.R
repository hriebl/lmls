all_attr_but_dim <- function(object, expected) {
  ignore_attr <- as.character(names(attributes(object)))
  ignore_attr <- c(ignore_attr, names(attributes(expected)))
  ignore_attr[ignore_attr != "dim"]
}

expect_exactly <- function(object,
                           expected,
                           ...,
                           ignore_attr = NULL,
                           tolerance = testthat::testthat_tolerance(),
                           info = NULL,
                           label = NULL,
                           expected.label = NULL) {
  if (is.null(ignore_attr)) {
    ignore_attr <- all_attr_but_dim(object, expected)
  }

  testthat::expect_equal(
    object,
    expected,
    ...,
    ignore_attr = ignore_attr,
    tolerance = tolerance,
    info = info,
    label = label,
    expected.label = expected.label
  )
}

expect_roughly <- function(object,
                           expected,
                           ...,
                           ignore_attr = NULL,
                           tolerance = 0.05,
                           info = NULL,
                           label = NULL,
                           expected.label = NULL) {
  if (is.null(ignore_attr)) {
    ignore_attr <- all_attr_but_dim(object, expected)
  }

  testthat::expect_equal(
    object,
    expected,
    ...,
    ignore_attr = ignore_attr,
    tolerance = tolerance,
    info = info,
    label = label,
    expected.label = expected.label
  )
}
