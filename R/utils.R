#' Count number of times one number is higher than others
#'
#' @param x Number to count
#' @param y Vector of numbers to compare
#' @param na.rm = Logical; should NA values in the comparison
#' vector be removed before making comparison?
#'
#' @return Number of times x is higher than y
#' @keywords internal
#' @examples
#' \dontrun{
#' count_higher(4, 1:10)
#' count_higher(4, c(1:10, NaN))
#' }
count_higher <- function(x, y, na.rm = TRUE) {

	assertthat::assert_that(assertthat::is.number(x))
	assertthat::assert_that(is.numeric(y))

	# remove any NAs before making comparison
	if (isTRUE(na.rm)) y <- y[!is.na(y)]

	# if comparison is zero length, return NA
	if (length(y) == 0) return(NaN)

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
#' @keywords internal
#' @examples
#' \dontrun{
#' count_lower(4, 1:10)
#' count_lower(NaN, 1:10)
#' }
count_lower <- function(x, y, na.rm = TRUE) {

	assertthat::assert_that(assertthat::is.number(x))
	assertthat::assert_that(is.numeric(y))

	# remove any NAs before making comparison
	if (isTRUE(na.rm)) y <- y[!is.na(y)]

	# if comparison is zero length, return NA
	if (length(y) == 0) return(NaN)

	sum((x < y))
}
