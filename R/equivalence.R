
#' Test for equivalence
#'
#' @description For each pair of values, `equivalent()` checks whether they
#'   evaluate equally. `equivalent_scalar()` is faster but only works with
#'   length-1 vectors.
#'
#'   By default:
#'   - Any two missing values count as equivalent.
#'   - One known and one missing value do not count as equivalent.
#'   - Two known values are checked for equivalence using [`==`].
#'
#'   The operators `%eq%` and `%eq1%` always use these defaults. `%eq%` is like
#'   `equivalent()`, and `%eq1%` is like `equivalent_scalar()`.
#'
#' @details These functions are useful if the focus is on the data themselves --
#'   not what the data might represent in the real world, or minor differences
#'   between their storage modes (e.g., numeric versus integer, `NA` versus
#'   `NA_character_`). If types or attributes do matter, there is still the
#'   `strict = TRUE` option.
#'
#'   ## Comparison to base R
#'   - Unlike [`==`], these functions treat `NA`s as genuine values, not as
#'   placeholders for unknown values: they consider `NA` and `NA` to be equal.
#'   - Unlike [`identical()`], they ignore attributes and type differences
#'   between `NA`s by default: they consider, e.g., `NA_real_` and `NA_integer_`
#'   to be equal. Also, `equivalent()` is vectorized, and `equivalent_scalar()`
#'   makes sure that both inputs have length 1. Both functions warn or error if
#'   the inputs have incompatible lengths, i.e., two lengths that are different
#'   from 1 and from each other.
#'   - Unlike [`all.equal()`], they are clear, simple, and consistent.
#'
#' @param x,y Two vectors. They should have the same length unless either has
#'   length 1. For `equivalent_scalar()`, both must have length 1.
#' @param strict Logical. If set to `TRUE`, values are checked for exact
#'   equivalence using [`identical()`]. Default is `FALSE`.
#'
#' @return Logical. Never `NA` unless `x` or `y` have length 0. For
#'   `equivalent()` and `%eq%`, the length depends on `x` and `y`, as in [`&`],
#'   [`==`], etc. For `equivalent_scalar()` and `%eq1%`, the length is always 1.
#'
#' @export
#'
#' @examples
#'
#' # Operators --------------------------------
#'
#' # `%eq%` applies `equivalent()`...
#'
#' # ...and `%eq1%` applies `equivalent_scalar()`:

# TODO: WRITE EXAMPLES

equivalent <- function(x, y, strict = FALSE) {
  if (strict) {
    return(mapply(identical, x, y, USE.NAMES = FALSE))
  }
  (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
}


# Why is the length check in `equivalent_scalar()` conditional on the `strict =
# TRUE` option? The whole point of this function -- as separate from
# `equivalent()` -- is speed, and in any case, an equivalent length check is
# implicit in the default behavior (although with a different error message). By
# contrast, `identical()` isn't vectorized, so it doesn't care about length in
# this way, and the check is needed to treat length consistently.

#' @rdname equivalent
#' @export
equivalent_scalar <- function(x, y, strict = FALSE) {
  if (strict) {
    if (length(x) != 1L || length(y) != 1L) {
      stop(paste(
        "Can only use `equivalent_scalar()` with length-1 vectors.\n",
        "To compare other objects, consider using `equivalent()`",
        "or `identical()` instead."
      ))
    }
    return(identical(x, y))
  }
  (is.na(x) && is.na(y)) || (!is.na(x) && !is.na(y) && x == y)
}



# equivalent_all <- function(x, y, strict = FALSE) {
#   if (strict) {
#     return(identical(x, y))
#   }
#   all(equivalent(x, y))
# }

#' @rdname equivalent
#' @export
`%eq%` <- function(x, y) {
  equivalent(x, y)
}

#' @rdname equivalent
#' @export
`%eq1%` <- function(x, y) {
  equivalent_scalar(x, y)
}




