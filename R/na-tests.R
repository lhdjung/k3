
#' Test for joint missingness
#'
#' @description For each pair of values, these functions run checks for missing
#'   values.
#'
#'   By default:
#'   - `equally_na()` checks that both values are `NA` (missing).
#'   - `equally_not_na()` checks that both values are not `NA`.
#'   - `equally_na_or_not_na()` checks that either both values are `NA`, or both
#'   are not `NA`.
#'
#' @param x,y Two vectors. They should have the same length unless either has
#'   length 1.
#' @param strict Logical. If set to `TRUE`, the missingness check results of
#'   each pair of values are checked for exact equality using [`identical()`].
#'   This includes equal attributes such as names. Default is `FALSE`.
#'
#' @return Logical. Never `NA`. The length depends on `x` and `y`, as in [`&`],
#'   [`==`], etc.
#'
#' @name na-tests
#'
#' @export
#'
#' @examples
equally_na <- function(x, y, strict = FALSE) {
  if (strict) {
    return(is.na(x) & is.na(y) & equal_attributes(x, y))
  }
  is.na(x) & is.na(y)
}


#' @rdname na-tests
#' @export
equally_not_na <- function(x, y, strict = FALSE) {
  if (strict) {
    return(!is.na(x) & !is.na(y) & equal_attributes(x, y))
  }
  !is.na(x) & !is.na(y)
}


#' @rdname na-tests
#' @export
equally_na_or_not_na <- function(x, y, strict = FALSE) {
  if (strict) {
    return(is.na(x) == is.na(y) & equal_attributes(x, y))
  }
  is.na(x) == is.na(y)
}


# Helper to check each value pair for equal attributes:
equal_attributes <- function(x, y) {
  mapply(identical, attributes(x), attributes(y), USE.NAMES = FALSE)
}


# Scalar variants ---------------------------------------------------------

# These don't seem like they have merit: they are a bit trivial and would bloat
# the package. The third one isn't even faster than its main function!

# equally_na_scalar <- function(x, y, strict = FALSE) {
#   if (strict) {
#     return(equally_na_strict(x, y))
#   }
#   is.na(x) && is.na(y)
# }
#
# equally_not_na_scalar <- function(x, y, strict = FALSE) {
#   if (strict) {
#     return(equally_not_na_strict(x, y))
#   }
#   !is.na(x) && !is.na(y)
# }
#
# equally_na_or_not_na_scalar <- function(x, y, strict = FALSE) {
#   if (strict) {
#     return(equally_na_or_not_na_strict(x, y))
#   }
#   TRUE && is.na(x) == is.na(y)
# }



# Strict-mode helpers -----------------------------------------------------

# TODO: Are these needed? I don't currently think so.

equally_na_strict <- function(x, y) {
  is.na(x) && is.na(y) && identical(is.na(x), is.na(y))
}

equally_not_na_strict <- function(x, y) {
  !is.na(x) && !is.na(y) && identical(is.na(x), is.na(y))
}

equally_na_or_not_na_strict <- function(x, y) {
  typeof(x) == typeof(y) && identical(is.na(x), is.na(y))
}
