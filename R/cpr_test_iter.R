#' Test the number of iterations needed to generate a random community that is
#'   sufficienty different from the original community.
#'
#' A single random community will be created for each value in `n_iter_test`,
#' using that value for the `n_iterations` argument of [cpr_rand_comm()]. The
#' Pearson correlation-coefficient is then calculated between the original
#' community and the random community.
#'
#' The user should verify that correlation coefficients decrease with increasing
#' iterations until an approximate minimum is reached.
#'
#' @srrstats {G2.0a, G2.1a, G2.3b} Documents expectations on lengths, types of
#'   vector inputs, case-sensitivity
#' @inheritParams cpr_rand_comm
#' @param n_iter_test Numeric vector; number of iterations to test.
#' @param time_digits Numeric vector of length 1; number of digits to include
#'   when recording elapsed time to calculate each random community. Default 5.
#' @param time_units Character vector of length 1; units to use when
#'   recording elapsed time to calculate each random community. Must be
#'   a valid value for "units" argument of [base::difftime()]. Default seconds
#'   ("secs").
#'
#' @return Tibble (dataframe) with the following columns:
#'   - `n_iter`: Number of iterations used to generate random community
#'   - `rand_comm`: List-column containing each random community
#'   - `corr`: Pearson correlation-coefficient between original community and
#'             random community
#'   - `time`: Amount of time elapsed when generating random community
#' @examples
#' data(biod_example)
#' cpr_test_iter(
#'   comm = biod_example$comm,
#'   null_model = "swap",
#'   n_iter_test = c(10e1, 10e2, 10e3, 10e4),
#'   seed = 123
#' )
#'
#' @autoglobal
#'
#' @srrstats {G1.4, G1.4a} uses roxygen
#'
cpr_test_iter <- function(
  comm, null_model, thin = 1, seed = NULL, n_iter_test,
  time_digits = 5, time_units = "secs") {

  #' @srrstats {G2.1, G2.6} Check input types and lengths
  # - comm
  assertthat::assert_that(
    inherits(comm, "data.frame") | inherits(comm, "matrix"),
    msg = "'comm' must be of class 'data.frame' or 'matrix'"
  )
  assertthat::assert_that(
    isTRUE(
      all(unique(purrr::map_chr(comm, class)) %in% c("numeric", "integer"))
    ),
    msg = "All columns of 'comm' must be numeric or integer class"
  )
  # - null_model
  assertthat::assert_that(
    assertthat::is.string(null_model) | inherits(null_model, "commsim"),
    msg = "'null_model' must be a string (character vector of length 1) or an object of class 'commsim'" # nolint
  )
  if (isTRUE(assertthat::is.string(null_model))) {
    assertthat::assert_that(assertthat::not_empty(comm))
    assertthat::assert_that(assertthat::noNA(null_model))
    assertthat::assert_that(
      isTRUE(null_model %in% vegan::make.commsim()),
      msg = paste0(
        "'null_model' must be one of: '",
        paste0(vegan::make.commsim(), collapse = "', '"), "'"
      )
    )
  }
  # - n_iter_test
  assertthat::assert_that(is.numeric(n_iter_test))
  assertthat::assert_that(assertthat::noNA(n_iter_test))
  assertthat::assert_that(all(is.finite(n_iter_test)))
  n_iter_test <- as.integer(n_iter_test)
  assertthat::assert_that(is.integer(n_iter_test))
  assertthat::assert_that(
    all(n_iter_test > 0),
    msg = "'n_iter_test' must be > 0")
  # - thin
  assertthat::assert_that(assertthat::is.number(thin))
  assertthat::assert_that(assertthat::noNA(thin))
  assertthat::assert_that(is.finite(thin))
  thin <- as.integer(thin)
  assertthat::assert_that(is.integer(thin))
  assertthat::assert_that(thin > 0, msg = "'thin' must be > 0")
  # - seed
  assertthat::assert_that(is.numeric(seed) | is.null(seed))
  # - time_digits
  assertthat::assert_that(assertthat::is.number(time_digits))
  assertthat::assert_that(assertthat::noNA(time_digits))
  time_digits <- as.integer(time_digits)  
  assertthat::assert_that(is.integer(time_digits))
  assertthat::assert_that(is.finite(time_digits))
  # - time_units
  assertthat::assert_that(assertthat::is.string(time_units))
  assertthat::assert_that(assertthat::noNA(time_units))

   # FIXME: detect if null model is binary or quantitative
   # "swap" produces a binary matrix, so convert the input matrix to binary too:
   comm <- as.matrix(comm)
   comm[comm > 0] <- 1

   # Define helper function to time cpr_rand_comm_intern
   cpr_rand_comm_intern_timed <- function(time_units, time_digits, ...) {
      start <- Sys.time()
      res <- cpr_rand_comm_intern(...)
      end <- Sys.time()
      time_elapsed <- difftime(end, start, units = time_units)
      time_elapsed <- round(time_elapsed, time_digits)
      time_elapsed <- as.numeric(time_elapsed)
      attributes(res) <- list(time = time_elapsed)
      res
    }

   # Use a tibble to hold the data and run loops
   tibble::tibble(
     n_iter = n_iter_test,
     # Make one random community for each value of `n_iter`
     rand_comm = purrr::map(
       n_iter,
       ~ cpr_rand_comm_intern_timed(
          comm = comm, null_model = null_model,
          n_iterations = .,
          thin = thin, seed = seed,
          time_digits = time_digits,
          time_units = time_units
          )
     ),
     # Calculate the Pearson correlation (r) between each randomized
     # matrix and the original matrix by converting
     # the matrices to vectors
     corr = purrr::map_dbl(
       rand_comm,
       ~ cor(c(comm), c(.)) # c() converts a matrix to vector
     ),
     time = purrr::map_dbl(
      rand_comm,
      ~ attributes(.)$time
     )
   )

}
