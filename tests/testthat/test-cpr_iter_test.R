#' @srrstats {G5.2, G5.2a, G5.2b, UL7.0} tests failure if input is not valid and
#' checks warning messages
test_that("Input is valid", {
  expect_error(
    cpr_iter_test(
      comm = data.frame(a = c("a", "b")),
      n_iter_test = c(10, 100)
    ),
    "All columns of 'comm' must be numeric or integer class",
    fixed = TRUE
  )
  expect_error(
    cpr_iter_test(
      comm = biod_example$comm,
      n_iter_test = c(-100, 100)
    ),
    "'n_iter_test' must be > 0"
  )
  expect_error(
    cpr_iter_test(
      comm = biod_example$comm,
      n_iter_test = c(10, 100),
      null_model = "foobar"
    ),
    "'null_model' must be one of"
  )
  expect_error(
    cpr_iter_test(
      comm = biod_example$comm,
      n_iter_test = c(10, 100),
      thin = -10
    ),
    "'thin' must be > 0"
  )
  expect_error(
    cpr_iter_test(
      comm = biod_example$comm,
      n_iter_test = c(10, 100),
      time_digits = -10
    ),
    "'time_digits' must be > 0"
  )
  expect_error(
    cpr_iter_test(
      comm = biod_example$comm,
      n_iter_test = c(10, 100),
      time_units = 10
    ),
    "time_units is not a string"
  )
  expect_warning(
    cpr_iter_test(
      comm = biod_example$comm,
      n_iter_test = c(10, 100),
      binary = TRUE,
      null_model = "swap_count"
    ),
    "The 'swap_count' null model is quantitative but you specified 'binary = TRUE'" # nolint
  )
  expect_warning(
    cpr_iter_test(
      comm = biod_example$comm,
      n_iter_test = c(10, 100),
      binary = FALSE,
      null_model = "swap"
    ),
    "The 'swap' null model is binary but you specified 'binary = FALSE'" # nolint
  )
})

#' @srrstats {G5.4, G5.5} Correctness tests
test_that("Output is formatted as expected", {
  # Run cpr_iter_test
  res <- cpr_iter_test(
    comm = biod_example$comm,
    binary = TRUE,
    n_iter_test = c(10e1, 10e2, 10e3),
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
    c("n_iter", "dist", "time")
  )
  expect_type(
    res$n_iter,
    "integer"
  )
  expect_type(
    res$dist,
    "double"
  )
  expect_type(
    res$time,
    "double"
  )
})

test_that("Random seeds work", {
  # Same seed produces same output
  res_1 <- cpr_iter_test(
    comm = biod_example$comm,
    n_iter_test = c(10e1, 10e2, 10e3),
    seed = 123
  )
  res_2 <- cpr_iter_test(
    comm = biod_example$comm,
    n_iter_test = c(10e1, 10e2, 10e3),
    seed = 123
  )
  res_3 <- cpr_iter_test(
    comm = biod_example$comm,
    n_iter_test = c(10e1, 10e2, 10e3),
    seed = 456
  )
  expect_equal(
    res_1$dist, res_2$dist
  )
  # Different seeds produce different output
  expect_false(
    isTRUE(
      all.equal(
        res_1$dist, res_3$dist
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
  cpr_iter_test(
    comm = biod_example$comm,
    n_iter_test = c(10e1, 10e2, 10e3),
    null_model = cs_object,
    seed = 123,
    binary = TRUE
  )
})
