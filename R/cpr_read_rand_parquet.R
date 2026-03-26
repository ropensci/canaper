#' Read randomization results from parquet file
#'
#' Loads randomization results from a parquet file created by
#' `cpr_write_rand_parquet()`. The results can then be used in downstream
#' analyses.
#'
#' @srrstats {G2.0a, G2.1a} Documents expectations on inputs
#' @param path Character vector of length 1; file path to the parquet file
#'   to read.
#' @param as_tibble Logical vector of length 1; if `TRUE`, return a tibble.
#'   If `FALSE`, return a data frame. Default is `FALSE`.
#'
#' @return A data frame or tibble (depending on `as_tibble`) with columns:
#'   - `site`: Character; site/community identifier
#'   - `replicate`: Integer; replicate number
#'   - Additional columns for each metric (e.g., `pd`, `pe`, `rpd`, `rpe`)
#'     as numeric vectors
#'
#' @examples
#' \dontshow{if (requireNamespace("arrow", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf}
#' # First, create a parquet file with mock data
#' sites <- c("site1", "site2", "site3")
#' set.seed(123)
#' rand_results <- lapply(1:10, function(i) {
#'   list(
#'     pd = runif(length(sites), 0, 1),
#'     pe = runif(length(sites), 0, 1),
#'     rpd = runif(length(sites), 0.5, 1.5)
#'   )
#' })
#'
#' temp_file <- tempfile(fileext = ".parquet")
#' cpr_write_rand_parquet(rand_results, sites, temp_file)
#'
#' # Read the randomization results as data frame
#' rand_data <- cpr_read_rand_parquet(temp_file)
#' head(rand_data)
#'
#' # Read as tibble instead
#' rand_tibble <- cpr_read_rand_parquet(temp_file, as_tibble = TRUE)
#' rand_tibble
#'
#' # Inspect structure
#' str(rand_data)
#'
#' # Check dimensions: should be n_sites * n_reps rows
#' nrow(rand_data) # 30 rows (3 sites * 10 replicates)
#'
#' # Extract data for a specific site and replicate
#' subset(rand_data, site == "site1" & replicate == 1)
#'
#' # Clean up
#' unlink(temp_file)
#' \dontshow{}) # examplesIf}
#'
#' @srrstats {G1.4, G1.4a} uses roxygen
#'
#' @export
cpr_read_rand_parquet <- function(path, as_tibble = FALSE) {
  # Check that arrow package is available
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop(
      "Package 'arrow' is required for parquet functionality. ",
      "Please install it with: install.packages('arrow')",
      call. = FALSE
    )
  }

  # Input validation ----
  #' @srrstats {G2.0, G2.1, G2.2, G2.3, G2.6} Check input types
  assertthat::assert_that(
    assertthat::is.string(path),
    msg = "'path' must be a character vector of length 1"
  )
  assertthat::assert_that(
    assertthat::is.flag(as_tibble),
    msg = "'as_tibble' must be TRUE or FALSE"
  )

  # Check that file exists
  assertthat::assert_that(
    file.exists(path),
    msg = paste0("File not found: ", path)
  )

  # Read parquet file ----
  rand_df <- arrow::read_parquet(path)

  # Validate schema ----
  # Required columns
  required_cols <- c("site", "replicate")
  col_names <- colnames(rand_df)

  for (req_col in required_cols) {
    assertthat::assert_that(
      req_col %in% col_names,
      msg = paste0(
        "Required column '",
        req_col,
        "' not found in parquet file. ",
        "Available columns: ",
        paste(col_names, collapse = ", ")
      )
    )
  }

  # Validate column types
  assertthat::assert_that(
    is.character(rand_df$site) || is.factor(rand_df$site),
    msg = "'site' column must be character or factor"
  )
  assertthat::assert_that(
    is.numeric(rand_df$replicate) || is.integer(rand_df$replicate),
    msg = "'replicate' column must be numeric or integer"
  )

  # Check for at least one metric column
  metric_cols <- setdiff(col_names, required_cols)
  assertthat::assert_that(
    length(metric_cols) > 0,
    msg = paste0(
      "No metric columns found in parquet file. ",
      "Expected at least one of: pd, pd_alt, rpd, pe, pe_alt, rpe"
    )
  )

  # Validate that metric columns are numeric
  for (metric_col in metric_cols) {
    assertthat::assert_that(
      is.numeric(rand_df[[metric_col]]),
      msg = paste0("Metric column '", metric_col, "' must be numeric")
    )
  }

  # Convert to requested output format ----
  if (isTRUE(as_tibble)) {
    rand_df <- tibble::as_tibble(rand_df)
  } else {
    # Ensure it's a plain data.frame, not a tibble
    rand_df <- as.data.frame(rand_df)
  }

  rand_df
}
