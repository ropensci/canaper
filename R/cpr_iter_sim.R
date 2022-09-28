#' Simulate the number of iterations needed to generate a random community that
#' is sufficiently different from the original community
#'
#' For randomization algorithms that involve swapping (iterations), there is no
#' way to know *a-priori* how many iterations are needed to sufficiently "mix"
#' the community data matrix. [cpr_iter_sim()] records the percentage similarity
#' between the original matrix and a matrix that has been randomized with
#' successive swapping iterations, at each iteration.
#'
#' The user should inspect the results to determine at what number of iterations
#' the original matrix and randomized matrix reach maximum dissimilarity (see
#' Examples). This number will strongly depend on the size and structure of the
#' original matrix. Large matrices with many zeros will likely take more
#' iterations, and even then still retain relatively high similarity between the
#' original matrix and the randomized matrix.
#'
#' Available memory may be quickly exhausted if many (e.g., tens or hundreds of
#' thousands, or more) of iterations are used with no thinning on large
#' matrices; use `thin` to only record a portion of the results and save on
#' memory.
#'
#' Of course, [cpr_iter_sim()] only makes sense for randomization algorithms
#' that use iterations.
#'
#' Only presence/absence information is used to calculate percentage similarity
#' between community matrices.
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
#' @param n_iterations Numeric vector of length 1; maximum number of iterations to
#'   conduct.
#' @param thin Numeric vector of length 1; frequency to record percentage
#'   similarity between original matrix and randomized matrix. Results will
#'   be recorded every `thin` iterations (see Details).
#'
#' @return Tibble (dataframe) with the following columns:
#'   - `iteration`: Number of iterations used to generate random community
#'   - `similarity`: Percentage similarity between original community and
#'             random community
#' @examples
#' # Simulate generation of a random community with maximum of 10,000
#' # iterations, recording similarity every 100 iterations
#' (res <- cpr_iter_sim(
#'   comm = biod_example$comm,
#'   null_model = "swap",
#'   n_iterations = 10000,
#'   thin = 100,
#'   seed = 123
#' ))
#'
#' # Plot reveals that ca. 1000 iterations are sufficient to
#' # completely mix random community
#' plot(res$iteration, res$similarity, type = "l")
#'
#' @autoglobal
#' @srrstats {G1.4, G1.4a} uses roxygen
#' @export
cpr_iter_sim <- function(comm, null_model = "curveball", n_iterations = 100,
                         thin = 1, seed = NULL) {
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
  # - n_iterations
  assertthat::assert_that(assertthat::is.number(n_iterations))
  assertthat::assert_that(assertthat::noNA(n_iterations))
  assertthat::assert_that(is.finite(n_iterations))
  n_iterations <- as.integer(n_iterations)
  assertthat::assert_that(is.integer(n_iterations))
  assertthat::assert_that(n_iterations > 0, msg = "'n_iterations' must be > 0")
  # - thin
  assertthat::assert_that(assertthat::is.number(thin))
  assertthat::assert_that(assertthat::noNA(thin))
  assertthat::assert_that(is.finite(thin))
  thin <- as.integer(thin)
  assertthat::assert_that(is.integer(thin))
  assertthat::assert_that(thin > 0, msg = "'thin' must be > 0")
  # - seed
  assertthat::assert_that(is.numeric(seed) | is.null(seed))
  # Convert to matrix
  comm <- as.matrix(comm)
  # Initiate null model
  null_model <- vegan::nullmodel(comm, null_model)
  # Randomize matrix
  sims <- stats::simulate(
    null_model,
    nsim = n_iterations / thin,
    thin = thin,
    seed = seed,
    burnin = 0
  )
  # Obtain vector of iteration number
  iteration <- seq(
    attributes(sims)$start,
    attributes(sims)$end,
    attributes(sims)$thin
  )
  # Calculate % similarity between initial community and each iteration
  comm_bin <- m2bin(comm)
  similarity <- NULL
  for (i in seq_len(dim(sims)[3])) {
    similarity[i] <- matrix_sim(comm_bin, m2bin(sims[, , i]))
  }
  # Format output
  tibble::tibble(
    iteration = iteration,
    similarity = similarity
  )
}

# Helper function: convert numeric matrix to binary
m2bin <- function(matrix) {
  matrix[matrix > 0] <- 1
  matrix
}

# Helper function for cpr_iter_test()
# Calculate percentage similarity between two
# matrices of the same dimensions
matrix_sim <- function(m1, m2) {
  v1 <- c(m1)
  n_agree <- sum(v1 == c(m2))
  n_agree / length(v1)
}
