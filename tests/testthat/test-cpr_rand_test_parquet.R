# Tests for save_rand and load_rand in cpr_rand_test

test_that("cpr_rand_test with save_rand writes parquet file", {
  skip_if_not_installed("arrow")

  # Use small test data
  data(biod_example)
  comm_small <- biod_example$comm[1:10, ]
  # Remove species with zero occurrences
  comm_small <- comm_small[, colSums(comm_small) > 0]
  phy_small <- ape::keep.tip(biod_example$phy, colnames(comm_small))

  temp_file <- tempfile(fileext = ".parquet")

  # Run with save_rand
  results <- cpr_rand_test(
    comm_small,
    phy_small,
    null_model = "swap",
    n_reps = 5,
    metrics = c("pd", "pe"),
    save_rand = temp_file,
    quiet = TRUE
  )

  # Check that file was created
  expect_true(file.exists(temp_file))

  # Check that file has expected structure
  rand_df <- arrow::read_parquet(temp_file)
  expect_equal(nrow(rand_df), 50) # 10 sites * 5 reps
  expect_true(all(c("site", "replicate", "pd", "pe") %in% colnames(rand_df)))

  # Clean up
  unlink(temp_file)
})

test_that("cpr_rand_test with load_rand reads parquet file", {
  skip_if_not_installed("arrow")

  # Use small test data
  data(biod_example)
  comm_small <- biod_example$comm[1:10, ]
  # Remove species with zero occurrences
  comm_small <- comm_small[, colSums(comm_small) > 0]
  phy_small <- ape::keep.tip(biod_example$phy, colnames(comm_small))

  temp_file <- tempfile(fileext = ".parquet")

  # First run with save_rand
  set.seed(123)
  results1 <- cpr_rand_test(
    comm_small,
    phy_small,
    null_model = "swap",
    n_reps = 5,
    metrics = c("pd", "pe"),
    save_rand = temp_file,
    quiet = TRUE
  )

  # Run again with load_rand
  results2 <- cpr_rand_test(
    comm_small,
    phy_small,
    null_model = "swap", # null_model still required but ignored
    n_reps = 100, # n_reps ignored when loading
    metrics = c("pd", "pe"),
    load_rand = temp_file,
    quiet = TRUE
  )

  # Results should be identical since using same random values
  expect_equal(results1$pd_obs, results2$pd_obs)
  expect_equal(results1$pe_obs, results2$pe_obs)
  expect_equal(results1$pd_rand_mean, results2$pd_rand_mean)
  expect_equal(results1$pe_rand_mean, results2$pe_rand_mean)

  # Clean up
  unlink(temp_file)
})

test_that("cpr_rand_test round-trip preserves results", {
  skip_if_not_installed("arrow")

  # Use small test data
  data(biod_example)
  comm_small <- biod_example$comm[1:12, ]
  # Remove species with zero occurrences
  comm_small <- comm_small[, colSums(comm_small) > 0]
  phy_small <- ape::keep.tip(biod_example$phy, colnames(comm_small))

  temp_file <- tempfile(fileext = ".parquet")

  # Run with save_rand
  set.seed(456)
  results_save <- cpr_rand_test(
    comm_small,
    phy_small,
    null_model = "swap",
    n_reps = 10,
    metrics = c("pd", "rpd", "pe", "rpe"),
    save_rand = temp_file,
    quiet = TRUE
  )

  # Load and re-run
  results_load <- cpr_rand_test(
    comm_small,
    phy_small,
    null_model = "swap",
    metrics = c("pd", "rpd", "pe", "rpe"),
    load_rand = temp_file,
    quiet = TRUE
  )

  # All columns should be identical
  expect_equal(results_save, results_load)

  # Clean up
  unlink(temp_file)
})

test_that("load_rand validates site names match", {
  skip_if_not_installed("arrow")

  # Use small test data
  data(biod_example)
  comm1 <- biod_example$comm[1:20, ]
  # Remove species with zero occurrences
  comm1 <- comm1[, colSums(comm1) > 0]
  comm2 <- biod_example$comm[21:40, ] # Different sites
  # Remove species with zero occurrences
  comm2 <- comm2[, colSums(comm2) > 0]
  # Make sure comm1 and comm2 have the same species
  common_spp <- intersect(colnames(comm1), colnames(comm2))
  comm1 <- comm1[, common_spp]
  comm2 <- comm2[, common_spp]
  # Remove sites with zero species after filtering
  comm1 <- comm1[rowSums(comm1) > 0, ]
  comm2 <- comm2[rowSums(comm2) > 0, ]
  phy_small <- ape::keep.tip(biod_example$phy, common_spp)

  temp_file <- tempfile(fileext = ".parquet")

  # Save with comm1
  cpr_rand_test(
    comm1,
    phy_small,
    null_model = "swap",
    n_reps = 5,
    metrics = c("pd"),
    save_rand = temp_file,
    quiet = TRUE
  )

  # Try to load with comm2 (different sites) - should error
  expect_error(
    cpr_rand_test(
      comm2,
      phy_small,
      null_model = "swap",
      metrics = c("pd"),
      load_rand = temp_file,
      quiet = TRUE
    ),
    "sites" # Either "number of sites" or "site names" error
  )

  # Clean up
  unlink(temp_file)
})

test_that("load_rand validates metrics are available", {
  skip_if_not_installed("arrow")

  # Use small test data
  data(biod_example)
  comm_small <- biod_example$comm[1:10, ]
  # Remove species with zero occurrences
  comm_small <- comm_small[, colSums(comm_small) > 0]
  phy_small <- ape::keep.tip(biod_example$phy, colnames(comm_small))

  temp_file <- tempfile(fileext = ".parquet")

  # Save with only pd metric
  cpr_rand_test(
    comm_small,
    phy_small,
    null_model = "swap",
    n_reps = 5,
    metrics = c("pd"),
    save_rand = temp_file,
    quiet = TRUE
  )

  # Try to load requesting pe metric - should error
  expect_error(
    cpr_rand_test(
      comm_small,
      phy_small,
      null_model = "swap",
      metrics = c("pd", "pe"), # pe not in file
      load_rand = temp_file,
      quiet = TRUE
    ),
    "Requested metrics not found in loaded file: pe"
  )

  # Clean up
  unlink(temp_file)
})

test_that("cannot specify both save_rand and load_rand", {
  skip_if_not_installed("arrow")

  data(biod_example)
  comm_small <- biod_example$comm[1:10, ]
  # Remove species with zero occurrences
  comm_small <- comm_small[, colSums(comm_small) > 0]
  phy_small <- ape::keep.tip(biod_example$phy, colnames(comm_small))

  temp_file1 <- tempfile(fileext = ".parquet")
  temp_file2 <- tempfile(fileext = ".parquet")

  # First create a file
  cpr_rand_test(
    comm_small,
    phy_small,
    null_model = "swap",
    n_reps = 5,
    metrics = c("pd"),
    save_rand = temp_file1,
    quiet = TRUE
  )

  # Try to use both save_rand and load_rand - should error
  expect_error(
    cpr_rand_test(
      comm_small,
      phy_small,
      null_model = "swap",
      n_reps = 5,
      metrics = c("pd"),
      save_rand = temp_file2,
      load_rand = temp_file1,
      quiet = TRUE
    ),
    "Cannot specify both 'save_rand' and 'load_rand'"
  )

  # Clean up
  unlink(temp_file1)
})

test_that("load_rand validates file exists", {
  data(biod_example)
  comm_small <- biod_example$comm[1:10, ]
  # Remove species with zero occurrences
  comm_small <- comm_small[, colSums(comm_small) > 0]
  phy_small <- ape::keep.tip(biod_example$phy, colnames(comm_small))

  expect_error(
    cpr_rand_test(
      comm_small,
      phy_small,
      null_model = "swap",
      metrics = c("pd"),
      load_rand = "nonexistent_file.parquet",
      quiet = TRUE
    ),
    "File specified in 'load_rand' not found"
  )
})

test_that("messages are displayed when quiet = FALSE", {
  skip_if_not_installed("arrow")

  data(biod_example)
  comm_small <- biod_example$comm[1:20, ]
  # Remove species with zero occurrences
  comm_small <- comm_small[, colSums(comm_small) > 0]
  phy_small <- ape::keep.tip(biod_example$phy, colnames(comm_small))

  temp_file <- tempfile(fileext = ".parquet")

  # Save with message
  expect_message(
    cpr_rand_test(
      comm_small,
      phy_small,
      null_model = "swap",
      n_reps = 3,
      metrics = c("pd"),
      save_rand = temp_file,
      quiet = FALSE
    ),
    "Saving randomization results"
  )

  # Load with message
  expect_message(
    cpr_rand_test(
      comm_small,
      phy_small,
      null_model = "swap",
      metrics = c("pd"),
      load_rand = temp_file,
      quiet = FALSE
    ),
    "Loading randomization results"
  )

  # Clean up
  unlink(temp_file)
})
