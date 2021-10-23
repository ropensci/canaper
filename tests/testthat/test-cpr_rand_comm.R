#' @srrstats {G5.2, G5.2a, G5.2b, UL7.0} tests failure if input is not valid and checks warning messages
test_that("Input is valid", {
  expect_error(
  	cpr_rand_comm(biod_example$comm, 1),
  	"'null_model' must be a string (character vector of length 1) or an object of class 'commsim'",
  	fixed = TRUE
  )
	expect_error(
		cpr_rand_comm(data.frame(a = c("a", "b")), 1),
		"All columns of 'comm' must be numeric or integer class",
		fixed = TRUE
	)
})

test_that("Row and column sums are preserved for FF null models", {
	# Binary null models convert matrix to binary,
	# so make a binary matrix as input
	binary_comm <- phylocom$comm
	binary_comm[binary_comm > 0] <- 1
	# Get original row and col sums
  col_sums_original <- colSums(binary_comm)
  row_sums_original <- rowSums(binary_comm)
  set.seed(12345)
  # Make random communities
  rand_swap <- cpr_rand_comm(binary_comm, "swap", 100)
  rand_tswap <- cpr_rand_comm(binary_comm, "tswap", 100)
  rand_curve <- cpr_rand_comm(binary_comm, "curveball", 100)
  rand_00 <- cpr_rand_comm(binary_comm, "r00", 100)
  # Check
  expect_equal(colSums(rand_swap), col_sums_original)
  expect_equal(rowSums(rand_swap), row_sums_original)
  expect_equal(colSums(rand_tswap), col_sums_original)
  expect_equal(rowSums(rand_tswap), row_sums_original)
  expect_equal(colSums(rand_curve), col_sums_original)
  expect_equal(rowSums(rand_curve), row_sums_original)
  # we *don't* expect them to be preserved for r00 model
  expect_false(isTRUE(all.equal(rowSums(rand_00), row_sums_original)))
  expect_false(isTRUE(all.equal(colSums(rand_00), col_sums_original)))
})

test_that("Different seeds produce different random communities", {
	set.seed(12345)
	rand_comm_1 <- cpr_rand_comm(phylocom$comm, "curveball", 100)
	set.seed(67890)
	rand_comm_2 <- cpr_rand_comm(phylocom$comm, "curveball", 100)
	expect_false(isTRUE(all.equal(rand_comm_1, rand_comm_2)))
	rand_comm_3 <- cpr_rand_comm(phylocom$comm, "curveball", 100, seed = 2020)
	rand_comm_4 <- cpr_rand_comm(phylocom$comm, "curveball", 100, seed = 2121)
	expect_false(isTRUE(all.equal(rand_comm_3, rand_comm_4)))
})

test_that("The same seed produces the same random communities", {
	set.seed(12345)
	rand_comm_1 <- cpr_rand_comm(phylocom$comm, "curveball", 100)
	set.seed(12345)
	rand_comm_2 <- cpr_rand_comm(phylocom$comm, "curveball", 100)
	expect_true(isTRUE(all.equal(rand_comm_1, rand_comm_2)))
	rand_comm_3 <- cpr_rand_comm(phylocom$comm, "curveball", 100, seed = 2020)
	rand_comm_4 <- cpr_rand_comm(phylocom$comm, "curveball", 100, seed = 2020)
	expect_true(isTRUE(all.equal(rand_comm_3, rand_comm_4)))
	set.seed(2323)
	rand_comm_5 <- cpr_rand_comm(phylocom$comm, "curveball", 100)
	set.seed(42)
	rand_comm_6 <- cpr_rand_comm(phylocom$comm, "curveball", 100, seed = 2323)
	expect_true(isTRUE(all.equal(rand_comm_5, rand_comm_6)))
})
