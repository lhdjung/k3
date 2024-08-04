
#' Correctly handle `NA` in `order()`
#'
#' @description Use `order2()` instead of [`order()`] if your data may contain
#'   missing values that shouldn't simply be assumed to be larger or smaller
#'   than the known values.
#'
#'   By default, missing values lead to an error. Specify `na.last` as in
#'   [`base::order()`] to override this.
#'
#' @param ...,na.last,decreasing,method Passed on to [`base::order()`]. Note
#'   that the default for `na.last` is `TRUE` in [`order()`], but `NULL` in
#'   `order2()`.
#'
#' @return Integer or double. See [`base::order()`] for details.
#'
#' @details `order2()` is just a wrapper around [`base::order()`]. There is no
#'   difference apart from missing value handling.
#'
#'   For the full rationale behind this function, see TODO: REFER TO BIG ARTICLE
#'   ONCE IT'S DONE; SECTION ON `order()`!
#'
#' @export
#'
#' @examples
order2 <- function(..., na.last = NULL, decreasing = FALSE,
                   method = c("auto", "shell", "radix")) {
  if (is.null(na.last) && anyNA(c(...))) {
    stop(paste(
      "`NA` is not allowed by default. You may use `na.last` to",
      "override this; see documentation for `order()`."
    ))
  }
  order(..., na.last = na.last, decreasing = decreasing, method = method)
}
