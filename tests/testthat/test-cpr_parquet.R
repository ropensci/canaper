# Test parquet I/O functions

test_that("cpr_write_rand_parquet works with valid input", {
  skip_if_not_installed("arrow")

  # Create mock randomization results
  sites <- c("site1", "site2", "site3")
  n_reps <- 5
  n_sites <- length(sites)

  # Create mock rand_results (like output from calc_biodiv_random)
  rand_results <- lapply(seq_len(n_reps), function(i) {
    list(
      pd = runif(n_sites),
      pe = runif(n_sites),
      rpd = runif(n_sites)
    )
  })

  # Write to temp file
  temp_file <- tempfile(fileext = ".parquet")
  result <- cpr_write_rand_parquet(rand_results, sites, temp_file)

  # Check that file was created
  expect_true(file.exists(temp_file))

  # Check that function returns path invisibly
  expect_equal(result, temp_file)

  # Clean up
  unlink(temp_file)
})

test_that("cpr_write_rand_parquet handles different metrics", {
  skip_if_not_installed("arrow")

  # Create mock data with different metric combinations
  sites <- c("site1", "site2")
  rand_results <- list(
    list(pd = c(0.5, 0.6), pe = c(0.3, 0.4)),
    list(pd = c(0.7, 0.8), pe = c(0.2, 0.5))
  )

  temp_file <- tempfile(fileext = ".parquet")
  cpr_write_rand_parquet(rand_results, sites, temp_file)

  # Read back and check structure
  result <- arrow::read_parquet(temp_file)
  expect_equal(nrow(result), 4) # 2 sites * 2 reps
  expect_true(all(c("site", "replicate", "pd", "pe") %in% colnames(result)))

  unlink(temp_file)
})

test_that("cpr_write_rand_parquet validates input", {
  skip_if_not_installed("arrow")

  sites <- c("site1", "site2")
  valid_rand_results <- list(
    list(pd = c(0.5, 0.6)),
    list(pd = c(0.7, 0.8))
  )

  # Empty rand_results
  expect_error(
    cpr_write_rand_parquet(list(), sites, tempfile()),
    "must not be empty"
  )

  # Sites with NA
  expect_error(
    cpr_write_rand_parquet(
      valid_rand_results,
      c("site1", NA),
      tempfile()
    ),
    "must not contain NA"
  )

  # Mismatched lengths
  bad_rand_results <- list(
    list(pd = c(0.5)), # Only 1 value, but 2 sites
    list(pd = c(0.7))
  )
  expect_error(
    cpr_write_rand_parquet(bad_rand_results, sites, tempfile()),
    "does not match length"
  )

  # Invalid path type
  expect_error(
    cpr_write_rand_parquet(valid_rand_results, sites, c("a", "b")),
    "character vector of length 1"
  )
})

test_that("cpr_read_rand_parquet reads data correctly", {
  skip_if_not_installed("arrow")

  # Create and write mock data
  sites <- c("site1", "site2", "site3")
  rand_results <- list(
    list(pd = c(0.5, 0.6, 0.7), pe = c(0.3, 0.4, 0.5)),
    list(pd = c(0.8, 0.9, 1.0), pe = c(0.2, 0.3, 0.4))
  )

  temp_file <- tempfile(fileext = ".parquet")
  cpr_write_rand_parquet(rand_results, sites, temp_file)

  # Read back
  result <- cpr_read_rand_parquet(temp_file)

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 6) # 3 sites * 2 reps
  expect_true(all(c("site", "replicate", "pd", "pe") %in% colnames(result)))
  expect_equal(unique(result$site), sites)
  expect_equal(sort(unique(result$replicate)), c(1, 2))

  unlink(temp_file)
})

test_that("cpr_read_rand_parquet returns tibble when requested", {
  skip_if_not_installed("arrow")

  sites <- c("site1", "site2")
  rand_results <- list(
    list(pd = c(0.5, 0.6)),
    list(pd = c(0.7, 0.8))
  )

  temp_file <- tempfile(fileext = ".parquet")
  cpr_write_rand_parquet(rand_results, sites, temp_file)

  # Read as tibble
  result <- cpr_read_rand_parquet(temp_file, as_tibble = TRUE)
  expect_s3_class(result, "tbl_df")

  # Read as data.frame
  result_df <- cpr_read_rand_parquet(temp_file, as_tibble = FALSE)
  expect_s3_class(result_df, "data.frame")
  expect_false(inherits(result_df, "tbl_df"))

  unlink(temp_file)
})

test_that("cpr_read_rand_parquet validates file exists", {
  skip_if_not_installed("arrow")

  expect_error(
    cpr_read_rand_parquet("nonexistent_file.parquet"),
    "File not found"
  )
})

test_that("cpr_read_rand_parquet validates schema", {
  skip_if_not_installed("arrow")

  # Create a parquet file with wrong schema
  temp_file <- tempfile(fileext = ".parquet")

  # Missing required column
  bad_df <- data.frame(
    site = c("site1", "site2"),
    # missing "replicate" column
    pd = c(0.5, 0.6)
  )
  arrow::write_parquet(bad_df, temp_file)

  expect_error(
    cpr_read_rand_parquet(temp_file),
    "Required column 'replicate' not found"
  )

  unlink(temp_file)
})

test_that("round-trip preserves data", {
  skip_if_not_installed("arrow")

  # Create mock data
  sites <- c("siteA", "siteB", "siteC")
  set.seed(123)
  rand_results <- lapply(1:10, function(i) {
    list(
      pd = runif(3, 0, 1),
      pd_alt = runif(3, 0, 1),
      rpd = runif(3, 0.5, 1.5),
      pe = runif(3, 0, 1),
      pe_alt = runif(3, 0, 1),
      rpe = runif(3, 0.5, 1.5)
    )
  })

  # Write
  temp_file <- tempfile(fileext = ".parquet")
  cpr_write_rand_parquet(rand_results, sites, temp_file)

  # Read
  result <- cpr_read_rand_parquet(temp_file)

  # Verify data integrity
  expect_equal(nrow(result), 30) # 3 sites * 10 reps
  expect_equal(ncol(result), 8) # site, replicate, + 6 metrics

  # Check that values are preserved (compare first rep, first site)
  first_row <- result[result$site == "siteA" & result$replicate == 1, ]
  expect_equal(first_row$pd, rand_results[[1]]$pd[1])
  expect_equal(first_row$pe, rand_results[[1]]$pe[1])
  expect_equal(first_row$rpd, rand_results[[1]]$rpd[1])

  unlink(temp_file)
})

test_that("functions give clear error when arrow not installed", {
  # This test is tricky to implement without actually unloading arrow
  # Skip for now, but document that manual testing is needed
  skip("Manual testing required for missing arrow package")
})
