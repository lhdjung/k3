
#' Correctly handle `NA` in `which()`
#'
#' @description Use `which2()` instead of [`which()`] if your data may contain
#'   missing values that shouldn't simply be ignored.
#'
#'   Specify `na.treat` to decide how to handle missing values. By default, they
#'   will lead to an error.
#'
#' @param x,arr.ind,useNames Passed down to [`base::which()`].
#' @param na.treat String. One of these options:
#' - `"error"` (the default) throws an error if any elements of `x` are `NA`.
#' - `"warning"` alerts you to `NA`s but handles them like `"false"`.
#' - `"true"` treats `NA`s as `TRUE`, so their indices are part of the output.
#' - `"false"` treats `NA`s as `FALSE`, so their indices are not part of the
#'   output. This works just like [`base::which()`].
#'
#' @return Integer vector. Its length is never greater than `length(x)`, and it
#'   never contains `NA`. See [`base::which()`] for details.
#'
#' @details `which2()` is just a wrapper around [`base::which()`]. There is no
#'   difference apart from missing value handling.
#'
#'   For the full rationale behind this function, see TODO: REFER TO BIG ARTICLE
#'   ONCE IT'S DONE; SECTION ON `which()`!
#'
#' @export
#'
#' @examples
which2 <- function(x, arr.ind = FALSE, useNames = TRUE,
                   na.treat = c("error", "warning", "true", "false")) {
  na.treat <- match.arg(na.treat)
  # Without `NA`, no need to do anything differently from `base::which()`.
  if (!anyNA(x)) {
    return(which(x, arr.ind = arr.ind, useNames = useNames))
  }
  # How to handle `NA` elements?
  if (na.treat == "error") {
    stop(paste(
      "`NA` is not allowed by default. Use `na.treat` to",
      "handle it like `TRUE` or `FALSE`, or to get a",
      "warning instead."
    ))
  } else if (na.treat == "warning") {
    warning("`NA` is present but ignored by `which2()`.")
  } else if (na.treat == "true") {
    x[is.na(x)] <- TRUE
  }
  # The "false" option is meant to work just like `base::which()`, so there is
  # no need to explicitly handle it here.
  which(x, arr.ind = arr.ind, useNames = useNames)
}
