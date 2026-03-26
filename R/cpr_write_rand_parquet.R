#' Write randomization results to parquet file
#'
#' Saves randomization results from `cpr_rand_test()` to a parquet file for
#' later use. This can be useful for large datasets where randomizations are
#' time-consuming and you want to reuse them across multiple sessions.
#'
#' @srrstats {G2.0a, G2.1a} Documents expectations on inputs
#' @param rand_results List; randomization results as generated internally by
#'   `cpr_rand_test()`. This is a list where each element corresponds to one
#'   randomization replicate and contains named vectors of metric values (e.g.,
#'   "pd", "pe", "rpd", "rpe").
#' @param sites Character vector; names of sites/communities corresponding
#'   to the values in each randomization result.
#' @param path Character vector of length 1; file path where parquet file
#'   should be written. Parent directory must exist.
#' @param compression Character vector of length 1; compression algorithm to
#'   use. Options include "uncompressed", "snappy", "gzip", "brotli", "zstd",
#'   "lz4", "lzo", "bz2". Default is "zstd" for good compression ratio and
#'   speed.
#' @param metadata List; optional metadata to store with the parquet file.
#'   Can include information like null_model, n_iterations, seed, etc.
#'
#' @return Invisibly returns the path to the created file.
#'
#' @examples
#' \dontshow{if (requireNamespace("arrow", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf}
#' # Create mock randomization results (like those from cpr_rand_test)
#' sites <- c("site1", "site2", "site3")
#' n_reps <- 10
#'
#' # Simulate randomization results for each replicate
#' # Each replicate contains metrics for all sites
#' set.seed(123)
#' rand_results <- lapply(seq_len(n_reps), function(i) {
#'   list(
#'     pd = runif(length(sites), 0, 1),
#'     pe = runif(length(sites), 0, 1),
#'     rpd = runif(length(sites), 0.5, 1.5),
#'     rpe = runif(length(sites), 0.5, 1.5)
#'   )
#' })
#'
#' # Write to a temporary parquet file
#' temp_file <- tempfile(fileext = ".parquet")
#' cpr_write_rand_parquet(
#'   rand_results = rand_results,
#'   sites = sites,
#'   path = temp_file,
#'   compression = "zstd"
#' )
#'
#' # Verify file was created
#' file.exists(temp_file)
#'
#' # Clean up
#' unlink(temp_file)
#' \dontshow{}) # examplesIf}
#'
#' @srrstats {G1.4, G1.4a} uses roxygen
#'
#' @export
cpr_write_rand_parquet <- function(
  rand_results,
  sites,
  path,
  compression = "zstd",
  metadata = NULL
) {
  # Check that arrow package is available
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop(
      "Package 'arrow' is required for parquet functionality. ",
      "Please install it with: install.packages('arrow')",
      call. = FALSE
    )
  }

  # Input validation ----
  #' @srrstats {G2.0, G2.1, G2.2, G2.3, G2.6} Check input types and lengths
  assertthat::assert_that(
    is.list(rand_results),
    msg = "'rand_results' must be a list"
  )
  assertthat::assert_that(
    length(rand_results) > 0,
    msg = "'rand_results' must not be empty"
  )
  assertthat::assert_that(
    is.character(sites),
    msg = "'sites' must be a character vector"
  )
  assertthat::assert_that(
    assertthat::noNA(sites),
    msg = "'sites' must not contain NA values"
  )
  assertthat::assert_that(
    assertthat::is.string(path),
    msg = "'path' must be a character vector of length 1"
  )
  assertthat::assert_that(
    assertthat::is.string(compression),
    msg = "'compression' must be a character vector of length 1"
  )

  # Validate structure of rand_results
  # Each element should be a named list with metric vectors
  first_result <- rand_results[[1]]
  assertthat::assert_that(
    is.list(first_result),
    msg = "Each element of 'rand_results' must be a list"
  )
  assertthat::assert_that(
    !is.null(names(first_result)),
    msg = "Each element of 'rand_results' must be a named list"
  )

  # Check that all metrics have the same length as sites
  metric_names <- names(first_result)
  for (metric in metric_names) {
    metric_length <- length(first_result[[metric]])
    assertthat::assert_that(
      metric_length == length(sites),
      msg = paste0(
        "Length of metric '",
        metric,
        "' (",
        metric_length,
        ") ",
        "does not match length of 'sites' (",
        length(sites),
        ")"
      )
    )
  }

  # Convert to long-format data frame ----
  n_reps <- length(rand_results)
  n_sites <- length(sites)

  # Initialize data frame with site and replicate columns
  df_list <- list(
    site = rep(sites, times = n_reps),
    replicate = rep(seq_len(n_reps), each = n_sites)
  )

  # Add metric columns
  for (metric in metric_names) {
    # Extract this metric from all replicates and concatenate
    metric_values <- purrr::map(rand_results, ~ .x[[metric]]) |>
      unlist()
    df_list[[metric]] <- metric_values
  }

  # Create data frame
  rand_df <- as.data.frame(df_list)

  # Note: Metadata storage in parquet files is currently not implemented
  # This would require using arrow's schema metadata features, which is
  # deferred to a future enhancement. The metadata parameter is kept for
  # API stability.

  # Write to parquet ----
  arrow::write_parquet(
    rand_df,
    sink = path,
    compression = compression
  )

  # Return path invisibly
  invisible(path)
}
