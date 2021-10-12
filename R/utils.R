#' Count number of times one number is higher than others
#'
#' @param x Number to count
#' @param y Vector of numbers to compare
#' @param na.rm = Logical; should NA values in the comparison
#' vector be removed before making comparison?
#'
#' @return Number of times x is higher than y
#'
#' @srrstats {G1.4, G1.4a} uses roxygen
#'
#' @examples
#' \dontrun{
#' count_higher(4, 1:10)
#' count_higher(4, c(1:10, NaN))
#' }
#' @noRd
count_higher <- function(x, y, na.rm = TRUE) {

  #' @srrstats {G2.1, G2.6} Check input types and lengths
  assertthat::assert_that(assertthat::is.number(x))
  assertthat::assert_that(is.numeric(y))

  # remove any NAs before making comparison
  if (isTRUE(na.rm)) y <- y[!is.na(y)]

  # if comparison is zero length, return NA
  if (length(y) == 0) {
    return(NaN)
  }

  sum((x > y))
}

#' Count number of times one number is lower than others
#'
#' @param x Number to count
#' @param y Vector of numbers to compare
#' @param na.rm = Logical; should NA values in the comparison
#' vector be removed before making comparison?
#'
#' @return Number of times x is lower than y
#'
#' @srrstats {G1.4, G1.4a} uses roxygen
#'
#' @examples
#' \dontrun{
#' count_lower(4, 1:10)
#' count_lower(NaN, 1:10)
#' }
#' @noRd
count_lower <- function(x, y, na.rm = TRUE) {

  #' @srrstats {G2.1, G2.6} Check input types and lengths
  assertthat::assert_that(assertthat::is.number(x))
  assertthat::assert_that(is.numeric(y))

  # remove any NAs before making comparison
  if (isTRUE(na.rm)) y <- y[!is.na(y)]

  # if comparison is zero length, return NA
  if (length(y) == 0) {
    return(NaN)
  }

  sum((x < y))
}

#' Version of `<` that allows for modifying tolerance of equality test (`all.equal()`)
#'
#' Non-vectorized version. Is `x` lesser than `y`?
#'
#' @srrstats {G3.0} Uses appropriate tolerances for approximate equality
#' @param x Numeric vector of length 1
#' @param y Numeric vector of length 1
#' @return Logical vector of length 1
#' @noRd
lesser_than_single <- function(x, y) {
  #' @srrstats {G2.1, G2.6} Check input types and lengths
  assertthat::assert_that(assertthat::is.number(x))
  assertthat::assert_that(assertthat::is.number(y))
  !isTRUE(all.equal(x, y)) && (x < y)
}

#' Version of `<` that allows for modifying tolerance of equality test (`all.equal()`)
#'
#' Vectorized version. Is `x` lesser than `y`?
#'
#' @srrstats {G3.0} Uses appropriate tolerances for approximate equality
#' @param x Numeric vector
#' @param y Numeric vector
#' @return Logical vector
#' @noRd
`%lesser%` <- function(x,y) {
  #' @srrstats {G2.1, G2.6} Check input types
  assertthat::assert_that(is.numeric(x))
  assertthat::assert_that(is.numeric(y))
  purrr::map2_lgl(x, y, lesser_than_single)
}

#' Version of `>` that allows for modifying tolerance of equality test (`all.equal()`)
#'
#' Non-vectorized version. Is `x` greater than `y`?
#'
#' @srrstats {G3.0} Uses appropriate tolerances for approximate equality
#' @param x Numeric vector of length 1
#' @param y Numeric vector of length 1
#' @return Logical vector of length 1
#' @noRd
greater_than_single <- function(x, y) {
  #' @srrstats {G2.1, G2.6} Check input types and lengths
  assertthat::assert_that(assertthat::is.number(x))
  assertthat::assert_that(assertthat::is.number(y))
  !isTRUE(all.equal(x, y)) && (x > y)
}

#' Version of `>` that allows for modifying tolerance of equality test (`all.equal()`)
#'
#' Vectorized version. Is `x` greater than `y`?
#'
#' @srrstats {G3.0} Uses appropriate tolerances for approximate equality
#' @param x Numeric vector
#' @param y Numeric vector
#' @return Logical vector
#' @noRd
`%greater%` <- function(x,y) {
  #' @srrstats {G2.1, G2.6} Check input types
  assertthat::assert_that(is.numeric(x))
  assertthat::assert_that(is.numeric(y))
  purrr::map2_lgl(x, y, greater_than_single)
}
