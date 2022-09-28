#' @srrstats {G5.2, G5.2a, G5.2b, UL7.0} tests failure if input is not valid and
#' checks warning messages
test_that("Input is valid", {
  expect_error(
    cpr_iter_sim(
      comm = data.frame(a = c("a", "b"))
    ),
    "All columns of 'comm' must be numeric or integer class",
    fixed = TRUE
  )
  expect_error(
    cpr_iter_sim(
      comm = biod_example$comm,
      n_iterations = -100
    ),
    "'n_iterations' must be > 0"
  )
  expect_error(
    cpr_iter_sim(
      comm = biod_example$comm,
      null_model = "foobar"
    ),
    "'null_model' must be one of"
  )
  expect_error(
    cpr_iter_sim(
      comm = biod_example$comm,
      thin = -10
    ),
    "'thin' must be > 0"
  )
})

#' @srrstats {G5.4, G5.5} Correctness tests
test_that("Output is formatted as expected", {
  # Run cpr_iter_sim
  res <- cpr_iter_sim(
    comm = biod_example$comm,
    seed = 123
  )
  # Expect tibble output
  expect_s3_class(
    res,
    "tbl_df"
  )
  expect_s3_class(
    res,
    "tbl"
  )
  expect_s3_class(
    res,
    "data.frame"
  )
  # Column names and types
  expect_equal(
    colnames(res),
    c("iteration", "similarity")
  )
  expect_type(
    res$iteration,
    "integer"
  )
  expect_type(
    res$similarity,
    "double"
  )
})

test_that("Random seeds work", {
  # Same seed produces same output
  res_1 <- cpr_iter_sim(
    comm = biod_example$comm,
    seed = 123
  )
  res_2 <- cpr_iter_sim(
    comm = biod_example$comm,
    seed = 123
  )
  res_3 <- cpr_iter_sim(
    comm = biod_example$comm,
    seed = 456
  )
  expect_equal(
    res_1$similarity, res_2$similarity
  )
  # Different seeds produce different output
  expect_false(
    isTRUE(
      all.equal(
        res_1$similarity, res_3$similarity
      )
    )
  )
})

test_that("Custom models work", {
  # Note that this null model doesn't use iterations though
  randomizer <- function(x, n, ...) {
    array(replicate(n, sample(x)), c(dim(x), n))
  }
  cs_object <- vegan::commsim(
    "r00_model",
    fun = randomizer, binary = TRUE,
    isSeq = FALSE, mode = "integer"
  )
  cpr_iter_sim(
    comm = biod_example$comm,
    null_model = cs_object,
    seed = 123
  )
})
