
#' Does an object behave as if Boolean?
#'
#' - `is_boolean_like()` checks if an object is a logical vector that only has
#' `TRUE` and / or `FALSE` values, not `NA`.
#' - `is_boolean_like_scalar()` additionally checks if the object has length 1:
#' is it a single `TRUE` or `FALSE` value?
#'
#' @details Although R uses three-valued logic instead of Booleans, it is often
#'   useful to emulate Boolean logic by excluding `NA`.
#'
#'   `is_boolean_like_scalar(x)` is equivalent to `isTRUE(x) || isFALSE(x)` but
#'   tends to be faster if the result is likely `FALSE`.
#'
#' @param x Any object.
#'
#' @name boolean
#'
#' @return Logical (length 1). `TRUE` or `FALSE`, never `NA`.
#'
#' @export
#'
#' @examples

is_boolean_like <- function(x) {
  is.logical(x) && !any(is.na(x))
}

#' @name boolean
#' @export
is_boolean_like_scalar <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

