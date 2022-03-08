# Make objects used across multiple tests ----

# keep in a list for easy cleanup
dat <- list()

# comm in tibble format
dat$comm_tbl <- biod_example$comm |>
  tibble::rownames_to_column("site") |>
  tibble::as_tibble()

# comm in tibble format, non-default site name
dat$comm_tbl_2 <- biod_example$comm |>
  tibble::rownames_to_column("sample") |>
  tibble::as_tibble()

# comm in matrix format
dat$comm_mat <- as.matrix(biod_example$comm)

# For testing that results are same regardless of input type
# need to have same random seed each time
set.seed(12345)
dat$res_from_df <- cpr_rand_test(
  biod_example$comm, biod_example$phy,
  null_model = "swap", n_reps = 10
)
set.seed(12345)
dat$res_from_tbl <- cpr_rand_test(
  dat$comm_tbl, biod_example$phy,
  null_model = "swap", n_reps = 10, tbl_out = FALSE
)
set.seed(12345)
dat$res_from_mat <- cpr_rand_test(
  dat$comm_mat, biod_example$phy,
  null_model = "swap", n_reps = 10
)

# For testing output format
dat$res_tbl_from_df <- cpr_rand_test(
  biod_example$comm, biod_example$phy,
  null_model = "swap", n_reps = 1, metrics = "pd", tbl_out = TRUE
)
dat$res_tbl_from_tbl <- cpr_rand_test(
  dat$comm_tbl, biod_example$phy,
  null_model = "swap", n_reps = 1, metrics = "pd"
)
dat$res_df_from_tbl <- cpr_rand_test(
  dat$comm_tbl, biod_example$phy,
  null_model = "swap", n_reps = 1, metrics = "pd", tbl_out = FALSE
)

# Run tests ----

#' @srrstats {G5.2, G5.2a, G5.2b, UL7.0} tests failure if input is not valid and
#' checks warning messages
#' @srrstats {G5.0} Use standard dataset for testing (Biodiverse dataset)
test_that("Input is valid", {
  expect_error(
    cpr_rand_test(
      biod_example$comm, biod_example$phy,
      n_reps = -10, null_model = "swap"
    ),
    "'n_reps' must be > 0"
  )
  expect_error(
    cpr_rand_test(
      biod_example$comm, biod_example$phy,
      n_iterations = -10, null_model = "swap"
    ),
    "'n_iterations' must be > 0"
  )
  expect_error(
    cpr_rand_test(
      10, biod_example$phy,
      metrics = "pd", null_model = "swap"
    ),
    "'comm' must be of class 'data\\.frame' or 'matrix'"
  )
  expect_error(
    cpr_rand_test(biod_example$comm, NA, metrics = "pd", null_model = "swap"),
    "'phy' must be a list of class 'phylo'"
  )
  expect_error(
    cpr_rand_test(
      biod_example$comm, biod_example$phy,
      metrics = "pg", null_model = "swap"
    ),
    "'metrics' may only include 'pd', 'rpd', 'pe', or 'rpe'"
  )
  # input data too small
  comm_small <- biod_example$comm[1:3, ]
  set.seed(12345)
  comm_small <- matrix(
    data = sample(c(0, 1), replace = TRUE, size = length(unlist(comm_small))),
    nrow = nrow(comm_small),
    ncol = ncol(comm_small)
  )
  rownames(comm_small) <- rownames(biod_example$comm[1:3, ])
  colnames(comm_small) <- colnames(biod_example$comm[1:3, ])
  comm_small <- comm_small[, colSums(comm_small) > 0]
  expect_error(
    cpr_rand_test(
      comm_small, biod_example$phy,
      null_model = "swap", quiet = TRUE
    ),
    "'comm' must include at least 5 sites"
  )
  expect_error(
    cpr_rand_test(
      biod_example$comm,
      ape::keep.tip(biod_example$phy, c("sp1", "sp2")),
      null_model = "swap", quiet = TRUE
    ),
    "'phy' and 'comm' must share at least 5 species in common"
  )
  # comm with numeric species names
  comm_num_names <- biod_example$comm
  colnames(comm_num_names) <- 1:dim(comm_num_names)[2]
  phy_num_names <- biod_example$phy
  phy_num_names$tip.label <- colnames(comm_num_names) # nolint
  expect_error(
    cpr_rand_test(as.matrix(comm_num_names),
      phy_num_names,
      metrics = "pd", null_model = "swap"
    ),
    "Column names of 'comm' changed after conversion from matrix to dataframe\\. Do any column names start with a number" # nolint
  )
  # comm with duplicated species names
  comm_dup_names <- biod_example$comm
  colnames(comm_dup_names)[2] <- "sp1"
  expect_error(
    cpr_rand_test(comm_dup_names,
      biod_example$phy,
      metrics = "pd", null_model = "swap"
    ),
    "'comm' must have unique column names"
  )
  # phy with duplicated species names
  phy_dup_names <- biod_example$phy
  phy_dup_names$tip.label[2] <- "sp1"
  expect_error(
    cpr_rand_test(biod_example$comm, phy_dup_names, null_model = "swap"),
    "All tip labels in 'phy' must be unique"
  )
  # comm with non-numeric data
  comm_non_numeric <- biod_example$comm
  comm_non_numeric[1, 1] <- "a"
  expect_error(
    cpr_rand_test(
      comm_non_numeric, biod_example$phy,
      metrics = "pd", null_model = "swap"
    ),
    "All columns of 'comm' must be numeric"
  )
  # comm with NA data
  comm_with_na <- biod_example$comm
  comm_with_na[1, 1] <- NA
  expect_error(
    cpr_rand_test(
      comm_with_na, biod_example$phy,
      metrics = "pd", null_model = "swap"
    ),
    "No missing values allowed in 'comm'"
  )
  # comm with negative data
  comm_with_negative <- biod_example$comm
  comm_with_negative[1, 1] <- -10
  expect_error(
    cpr_rand_test(
      comm_with_negative,
      biod_example$phy,
      metrics = "pd", null_model = "swap"
    ),
    "No negative values allowed in 'comm'"
  )
  #' @srrstats {G2.11} non-vector inputs
  comm_with_nonvec <- data.frame(biod_example$comm)
  attributes(comm_with_nonvec[, 1]) <- list(bar = "foo")
  expect_false(
    is.vector(comm_with_nonvec[, 1])
  )
  expect_error(
    cpr_rand_test(
      comm_with_nonvec, biod_example$phy,
      metrics = "pd", null_model = "swap"
    ),
    "All columns of 'comm' must be numeric"
  )
  #' @srrstats {G2.16} infinite values
  comm_with_infinite <- biod_example$comm
  comm_with_infinite[1, 1] <- Inf
  expect_error(
    cpr_rand_test(
      comm_with_infinite,
      biod_example$phy,
      metrics = "pd", null_model = "swap"
    ),
    "No infinite values allowed in 'comm'"
  )
  expect_error(
    cpr_rand_test(
      dat$comm_tbl,
      biod_example$phy,
      metrics = "pd", site_col = "sample", null_model = "swap"
    ),
    "'site_col' must be one of the column names of 'comm'"
  )
  #' @srrstats {UL1.4} Check assumptions made with regard to input data
  # (negative branch lengths)
  phy_neg <- biod_example$phy
  phy_neg$edge.length[1] <- -1 # nolint
  expect_error(
    cpr_rand_test(biod_example$comm, phy_neg, null_model = "swap"),
    "'phy' may not have negative branchlengths"
  )
  phy_inf <- biod_example$phy
  phy_inf$edge.length[1] <- Inf # nolint
  expect_error(
    cpr_rand_test(biod_example$comm, phy_inf, null_model = "swap"),
    "'phy' may not have infinite branchlengths"
  )
  # check column with all zeros
  comm_one_zero_col <- biod_example$comm
  comm_one_zero_col[, 1] <- 0
  expect_error(
    cpr_rand_test(comm_one_zero_col, biod_example$phy, null_model = "swap"),
    "Every species in 'comm' must occur in at least one site"
  )
  # check row with all zeros
  comm_one_zero_row <- biod_example$comm
  comm_one_zero_row[1, ] <- 0
  expect_error(
    cpr_rand_test(comm_one_zero_row, biod_example$phy, null_model = "swap"),
    "Every site in 'comm' must have at least once species"
  )
  # input data with too many zeros or 1s
  # - make comm with >99.5% zeros
  set.seed(12345)
  comm_zero_heavy <- matrix(
    data = sample.int(
      2,
      replace = TRUE,
      prob = c(0.005, 0.995), size = length(unlist(biod_example$comm))
    ),
    nrow = nrow(biod_example$comm),
    ncol = ncol(biod_example$comm)
  )
  comm_zero_heavy[comm_zero_heavy == 2] <- 0
  # - make sure it doesn't break other rules (at least one species / site)
  comm_zero_heavy[1, ] <- 1
  comm_zero_heavy[, 1] <- 1
  rownames(comm_zero_heavy) <- rownames(biod_example$comm)
  colnames(comm_zero_heavy) <- colnames(biod_example$comm)
  expect_warning(
    cpr_rand_test(
      comm_zero_heavy, biod_example$phy,
      null_model = "r00", metrics = "pd"
    ),
    "'comm' is > 95% absences (zeros). Be sure that 'n_reps' and 'n_iterations' are sufficiently large to ensure adequate mixing of random communities", # nolint
    fixed = TRUE
  )
  # - quiet = TRUE should suppress the warning
  expect_warning(
    cpr_rand_test(
      comm_zero_heavy, biod_example$phy,
      null_model = "r00", metrics = "pd", quiet = TRUE
    ),
    NA
  )
  # - convert comm_zero_heavy to comm_one_heavy,
  # making sure it doesn't break other rules (at least one species / site)
  comm_one_heavy <- comm_zero_heavy
  comm_one_heavy[comm_one_heavy == 1] <- 2
  comm_one_heavy[comm_one_heavy == 0] <- 1
  comm_one_heavy[comm_one_heavy == 2] <- 0
  comm_one_heavy[1, ] <- 1
  comm_one_heavy[, 1] <- 1
  expect_warning(
    cpr_rand_test(
      comm_one_heavy, biod_example$phy,
      null_model = "r00", metrics = "pd"
    ),
    "'comm' is > 95% presences (values > 1). Be sure that 'n_reps' and 'n_iterations' are sufficiently large to ensure adequate mixing of random communities", # nolint
    fixed = TRUE
  )
  # - quiet = TRUE should suppress the warning
  expect_warning(
    cpr_rand_test(
      comm_one_heavy, biod_example$phy,
      null_model = "r00", metrics = "pd", quiet = TRUE
    ),
    NA
  )
})

test_that("Results are same regardless of input type", {
  expect_equal(dat$res_from_mat, dat$res_from_df)
  expect_equal(dat$res_from_mat, dat$res_from_tbl)
  expect_equal(dat$res_from_df, dat$res_from_tbl)
})

test_that("Results are same regardless of pres/abs or abundance input", {
  # make presence-absence community
  comm_pa <- apply(biod_example$comm, 2, function(x) ifelse(x > 0, 1, 0))
  # pd
  set.seed(12345)
  res_abun_pd <- cpr_rand_test(
    biod_example$comm, biod_example$phy,
    null_model = "swap", n_reps = 10, metrics = "pd"
  )
  set.seed(12345)
  res_pa_pd <- cpr_rand_test(
    comm_pa, biod_example$phy,
    null_model = "swap",
    n_reps = 10, metrics = "pd"
  )
  expect_equal(res_abun_pd, res_pa_pd)
  # pe
  set.seed(12345)
  res_abun_pe <- cpr_rand_test(
    biod_example$comm, biod_example$phy,
    null_model = "swap", n_reps = 10, metrics = "pe"
  )
  set.seed(12345)
  res_pa_pe <- cpr_rand_test(
    comm_pa, biod_example$phy,
    null_model = "swap", n_reps = 10, metrics = "pe"
  )
  expect_equal(res_abun_pe, res_pa_pe)
  # rpd
  set.seed(12345)
  res_abun_rpd <- cpr_rand_test(
    biod_example$comm, biod_example$phy,
    null_model = "swap", n_reps = 10, metrics = "rpd"
  )
  set.seed(12345)
  res_pa_rpd <- cpr_rand_test(
    comm_pa, biod_example$phy,
    null_model = "swap", n_reps = 10, metrics = "rpd"
  )
  expect_equal(res_abun_rpd, res_pa_rpd)
  # rpe
  set.seed(12345)
  res_abun_rpe <- cpr_rand_test(
    biod_example$comm, biod_example$phy,
    null_model = "swap", n_reps = 10, metrics = "rpe"
  )
  set.seed(12345)
  res_pa_rpe <- cpr_rand_test(
    comm_pa, biod_example$phy,
    null_model = "swap", n_reps = 10, metrics = "rpe"
  )
  expect_equal(res_abun_rpe, res_pa_rpe)
  # all
  set.seed(12345)
  res_abun <- cpr_rand_test(
    biod_example$comm, biod_example$phy,
    null_model = "swap", n_reps = 10
  )
  set.seed(12345)
  res_pa <- cpr_rand_test(
    comm_pa, biod_example$phy,
    null_model = "swap", n_reps = 10
  )
  expect_equal(res_abun, res_pa)
})

#' @srrstats {UL1.2} Check for default-looking column and rownames
test_that("Default column and rownames are detected", {
  # Make a dummy community dataframe and phylogeny
  set.seed(12345)
  abuns <- runif(10 * 8, 0, 10) |> as.integer()
  df_default_rows <- data.frame(
    sp1 = abuns[1:10],
    sp2 = abuns[11:20],
    sp3 = abuns[21:30],
    sp4 = abuns[31:40],
    sp5 = abuns[41:50],
    sp6 = abuns[51:60],
    sp7 = abuns[61:70],
    sp8 = abuns[71:80]
  )
  phy_default <- ape::keep.tip(biod_example$phy, paste0("sp", 1:8))
  expect_error(
    cpr_rand_test(df_default_rows, phy_default, null_model = "swap"),
    "'comm' cannot have default row names \\(consecutive integers from 1 to the number of rows\\)" # nolint
  )
})

#' @srrstats {UL7.5, UL7.5a} Test batch processing, show that results don't
#'   differ between sequential and parallel
test_that("Parallelization decreases calculation time", {
  # Change back to sequential when done (including on failure)
  on.exit(future::plan(future::sequential), add = TRUE)

  # Set future resolution to sequential (no parallelization)
  future::plan(future::sequential)
  tictoc::tic.clearlog()
  tictoc::tic()
  set.seed(12345)
  seq_res <- cpr_rand_test(
    acacia$comm, acacia$phy,
    null_model = "curveball",
    n_iterations = 100, n_reps = 100, quiet = TRUE
  )
  tictoc::toc(log = TRUE, quiet = TRUE)

  # Set future resolution to parallelized, with 3 workers
  future::plan(future::multisession, workers = 3)
  tictoc::tic()
  set.seed(12345)
  parallel_res <- cpr_rand_test(
    acacia$comm, acacia$phy,
    null_model = "curveball",
    n_iterations = 100, n_reps = 100, quiet = TRUE
  )
  tictoc::toc(log = TRUE, quiet = TRUE)
  log_list <- tictoc::tic.log(format = FALSE)
  tictoc::tic.clearlog()
  # Check times
  elapsed_time_seq <- log_list[[1]]$toc - log_list[[1]]$tic
  elapsed_time_parallel <- log_list[[2]]$toc - log_list[[2]]$tic

  # Change back to sequential
  future::plan(future::sequential)

  # Expect sequential to take longer
  expect_gt(elapsed_time_seq, elapsed_time_parallel)
  expect_lt(elapsed_time_parallel, elapsed_time_seq)
})

#' @srrstats {UL7.5, UL7.5a} Test batch processing, show that results don't
#'   differ between sequential and parallel
test_that("Seeds work across sequential and parallel", {
  # Change back to sequential when done (including on failure)
  on.exit(future::plan(future::sequential), add = TRUE)

  # Set future resolution to sequential (no parallelization)
  future::plan(future::sequential)
  set.seed(12345)
  seq_res_1 <- cpr_rand_test(
    biod_example$comm, biod_example$phy,
    null_model = "curveball", n_iterations = 10, n_reps = 10
  )
  set.seed(67890)
  seq_res_2 <- cpr_rand_test(
    biod_example$comm, biod_example$phy,
    null_model = "curveball",
    n_iterations = 10, n_reps = 10
  )
  set.seed(12345)
  seq_res_3 <- cpr_rand_test(
    biod_example$comm, biod_example$phy,
    null_model = "curveball",
    n_iterations = 10, n_reps = 10
  )

  expect_true(isTRUE(all.equal(seq_res_1, seq_res_3)))
  expect_false(isTRUE(all.equal(seq_res_1, seq_res_2)))

  # Set future resolution to parallelized, with 3 workers
  future::plan(future::multisession, workers = 3)
  # FIXME: for some bizarre reason, need to "prime" seed with a single run first
  # need to come up with a better solution
  par_res_0 <- cpr_rand_test(
    phylocom$comm, phylocom$phy,
    null_model = "curveball", n_iterations = 10,
    n_reps = 10, quiet = TRUE
  )
  # then the rest of the set.seed() calls work
  set.seed(12345)
  par_res_1 <- cpr_rand_test(
    biod_example$comm, biod_example$phy,
    null_model = "curveball",
    n_iterations = 10, n_reps = 10
  )
  set.seed(67890)
  par_res_2 <- cpr_rand_test(
    biod_example$comm, biod_example$phy,
    null_model = "curveball",
    n_iterations = 10, n_reps = 10
  )
  set.seed(12345)
  par_res_3 <- cpr_rand_test(
    biod_example$comm, biod_example$phy,
    null_model = "curveball",
    n_iterations = 10, n_reps = 10
  )

  expect_true(isTRUE(all.equal(par_res_1, par_res_3)))
  expect_false(isTRUE(all.equal(par_res_1, par_res_2)))
  expect_true(isTRUE(all.equal(seq_res_1, par_res_1)))

  # Change back to sequential
  future::plan(future::sequential)
})

#' @srrstats {G5.4, G5.4b, G5.5} Correctness tests
# Make sure results from canaper match those of Biodiverse
# (for non-random results only)
test_that("Output is same as when calculated with Biodiverse", {
  # Calculate observed PD, PE, etc using biodiverse test data
  set.seed(12345)
  res_compare <- cpr_rand_test(
    biod_example$comm, biod_example$phy,
    n_reps = 1, null_model = "swap"
  ) |>
    tibble::rownames_to_column("site") |>
    tibble::as_tibble() |>
    dplyr::select(site, matches("_obs$")) |>
    # Join on independently calculated results from Biodiverse
    dplyr::left_join(biod_results, by = "site") |>
    # Make sure no values are NA
    assertr::assert(assertr::not_na, dplyr::everything())
  expect_equal(res_compare$pd_obs, res_compare$pd_biodiv)
  expect_equal(res_compare$pd_alt_obs, res_compare$pd_alt_biodiv)
  expect_equal(res_compare$rpd_obs, res_compare$rpd_biodiv)
  expect_equal(res_compare$pe_obs, res_compare$pe_biodiv)
  expect_equal(res_compare$pe_alt_obs, res_compare$pe_alt_biodiv)
  expect_equal(res_compare$rpe_obs, res_compare$rpe_biodiv)
})

test_that("Output is different with different random seeds, and same with same random seed", { # nolint
  set.seed(12345)
  res_1 <- cpr_rand_test(
    biod_example$comm, biod_example$phy,
    n_reps = 100, null_model = "curveball"
  )
  set.seed(67890)
  res_2 <- cpr_rand_test(
    biod_example$comm, biod_example$phy,
    n_reps = 100, null_model = "curveball"
  )
  set.seed(12345)
  res_3 <- cpr_rand_test(
    biod_example$comm, biod_example$phy,
    n_reps = 100, null_model = "curveball"
  )
  expect_false(isTRUE(all.equal(res_1, res_2)))
  expect_true(isTRUE(all.equal(res_1, res_3)))
})

test_that("Output is formatted as expected", {
  # Use expect equal() with class() when *only* expecting a data.frame
  # (tibble inherits data.frame, tbl, and tbl_df)
  expect_equal(
    class(dat$res_from_df),
    "data.frame"
  )
  expect_s3_class(
    dat$res_tbl_from_df,
    "tbl_df"
  )
  expect_s3_class(
    dat$res_tbl_from_df,
    "tbl"
  )
  expect_s3_class(
    dat$res_tbl_from_tbl,
    "tbl_df"
  )
  expect_s3_class(
    dat$res_tbl_from_tbl,
    "tbl"
  )
  expect_equal(
    class(dat$res_df_from_tbl),
    "data.frame"
  )
  #' @srrstats {UL1.3, UL7.3} Make sure data from 'site_col' is carried through
  #' for tbl input
  res_tbl_from_tbl_2 <- cpr_rand_test(
    dat$comm_tbl_2, biod_example$phy,
    null_model = "swap",
    n_reps = 1, metrics = "pd", site_col = "sample"
  )
  expect_equal(
    dat$comm_tbl_2$sample,
    res_tbl_from_tbl_2$sample
  )
  expect_equal(
    rownames(dat$res_from_df),
    rownames(biod_example$comm)
  )
  # Run snapshot test to verify correct output
  set.seed(12345)
  expect_snapshot(
    cpr_rand_test(
      phylocom$comm, phylocom$phy,
      null_model = "swap",
      n_reps = 10, quiet = TRUE
    )
  )
})

test_that("Various randomization algorithms work", {
  set.seed(12345)
  algos <- vegan::make.commsim()
  res_list <- purrr::map(
    algos,
    ~ cpr_rand_test(
      biod_example$comm, biod_example$phy,
      null_model = ., n_iterations = 10, n_reps = 10
    )
  )
  # All output should be data frame
  for (i in seq_along(res_list)) {
    expect_s3_class(res_list[[i]], "data.frame")
  } # All values should be numeric
  for (i in seq_along(res_list)) {
    expect_true(
      assertr::assert(
        res_list[[i]], is.numeric, dplyr::everything(),
        success_fun = assertr::success_logical
      )
    )
  }
})

test_that("Custom randomization algorithms work", {
  randomizer <- function(x, n, ...) {
    array(replicate(n, sample(x)), c(dim(x), n))
  }
  cs_object <- vegan::commsim(
    "r00_model",
    fun = randomizer, binary = TRUE,
    isSeq = FALSE, mode = "integer"
  )
  set.seed(12345)
  expect_snapshot(
    cpr_rand_test(
      phylocom$comm, phylocom$phy, cs_object,
      n_reps = 10, quiet = TRUE
    )
  )
})

# Cleanup ----
remove(dat)
