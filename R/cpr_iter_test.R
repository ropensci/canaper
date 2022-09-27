#' Test the number of iterations needed to generate a random community that is
#'   sufficiently different from the original community.
#'
#' A single random community will be created for each value in `n_iter_test`,
#' using that value for the `n_iterations` argument of [cpr_rand_comm()]. The
#' Euclidean distance is then calculated between the original
#' community and each random community.
#'
#' The user should verify that distances increase with increasing
#' iterations until an approximate maximum is reached. Of course, this only
#' makes sense for randomization algorithms that use iterations.
#'
#' The number of iterations to test (`n_iter_test`) is left to the user. A good
#' rule of thumb is to test successive powers of 10. In this case, no more
#' than four or five values is usually needed before distances stabilize (see
#' Examples). Generally, fewer iterations are needed for smaller datasets.
#'
#' @srrstats {G2.0a, G2.1a, G2.3b} Documents expectations on lengths, types of
#'   vector inputs, case-sensitivity
#' @inheritParams cpr_rand_comm
#' @param null_model Character vector of length 1 or object of class `commsim`;
#'   either the name of the model to use for generating random communities (null
#'   model), or a custom null model. For full list of available predefined null
#'   models, see the help file of [vegan::commsim()], or run
#'   [vegan::make.commsim()]. An object of class `commsim` can be generated with
#'   [vegan::commsim()].
#' @param binary Logical vector of length 1; is null model a binary model
#'   (produces random community data matrix with 1s and 0s) or not?
#'   If NULL (default), will attempt to determine automatically from name of
#'   null model (`null_model`).
#' @param n_iter_test Numeric vector; number of iterations to test (see Details
#'   and Examples).
#' @param time_digits Numeric vector of length 1; number of digits to include
#'   when recording elapsed time to calculate each random community. Default 5.
#' @param time_units Character vector of length 1; units to use when
#'   recording elapsed time to calculate each random community. Must be
#'   a valid value for `units` argument of [base::difftime()]. Default seconds
#'   (`"secs"``).
#'
#' @return Tibble (dataframe) with the following columns:
#'   - `n_iter`: Number of iterations used to generate random community
#'   - `dist`: Euclidean distance between original community and
#'             random community
#'   - `time`: Amount of time elapsed when generating random community
#' @examples
#' data(biod_example)
#' cpr_iter_test(
#'   comm = biod_example$comm,
#'   null_model = "swap",
#'   n_iter_test = c(1, 10, 1000, 10000),
#'   seed = 123
#' )
#'
#' @autoglobal
#' @srrstats {G1.4, G1.4a} uses roxygen
#' @export
cpr_iter_test <- function(comm, null_model = "curveball", binary = NULL,
                          thin = 1, seed = NULL, n_iter_test,
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
  # - binary
  if (!is.null(binary)) {
    assertthat::assert_that(assertthat::is.flag(binary))
  }
  # - n_iter_test
  assertthat::assert_that(is.numeric(n_iter_test))
  assertthat::assert_that(assertthat::noNA(n_iter_test))
  assertthat::assert_that(all(is.finite(n_iter_test)))
  n_iter_test <- as.integer(n_iter_test)
  assertthat::assert_that(is.integer(n_iter_test))
  assertthat::assert_that(
    all(n_iter_test > 0),
    msg = "'n_iter_test' must be > 0"
  )
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
  assertthat::assert_that(time_digits > 0, msg = "'time_digits' must be > 0")
  # - time_units
  assertthat::assert_that(assertthat::is.string(time_units))
  assertthat::assert_that(assertthat::noNA(time_units))

  # Detect if null model is binary or quantitative
  binary_models <- c(
    "r00", "r0", "r1", "r2", "c0", "swap", "tswap",
    "curveball", "quasiswap", "greedyqswap", "backtracking"
  )

  quant_models <- c(
    "r2dtable", "quasiswap_count",
    "swap_count", "abuswap_r", "abuswap_c",
    "swsh_samp", "swsh_both", "swsh_samp_r", "swsh_samp_c", "swsh_both_r",
    "swsh_both_c",
    "r00_ind", "r0_ind", "c0_ind", "r00_samp", "r0_samp", "c0_samp",
    "r00_both", "r0_both", "c0_both"
  )

  if (is.null(binary)) {
    if (is.character(null_model) && null_model %in% binary_models) {
      binary <- TRUE
    } else if (is.character(null_model) && null_model %in% quant_models) {
      binary <- FALSE
    } else {
      stop("Cannot determine if null model is binary or not. Specify with 'binary' argument.") # nolint
    }
  } else {
    if (is.character(null_model) &&
      null_model %in% binary_models && binary == FALSE) {
      warning(paste0(
        "The '", null_model,
        "' null model is binary but you specified 'binary = FALSE'"
      ))
    }
    if (is.character(null_model) &&
      null_model %in% quant_models && binary == TRUE) {
      warning(paste0(
        "The '", null_model,
        "' null model is quantitative but you specified 'binary = TRUE'"
      ))
    }
  }

  # Convert input community to matrix
  comm <- as.matrix(comm)

  # binary models produces a binary matrix
  # so convert the input matrix to binary for binary null models
  if (isTRUE(binary)) {
    comm[comm > 0] <- 1
  }

  # Use a tibble to hold the data and run loops
  res <- tibble::tibble(
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
    # Calculate the Euclidean distance between each randomized
    # matrix and the original matrix by converting
    # the matrices to vectors
    dist = purrr::map_dbl(
      rand_comm,
      ~ dist(rbind(c(comm), c(.)))
    ),
    time = purrr::map_dbl(
      rand_comm,
      ~ attributes(.)$time
    )
  )
  # Drop rand_comm column
  res[colnames(res) != "rand_comm"]
}
