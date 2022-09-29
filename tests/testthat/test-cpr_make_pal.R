#' @srrstats {G5.2, G5.2a, G5.2b, UL7.0} tests failure if input is not valid and
#' checks warning messages
test_that("Input is valid", {
  expect_error(
    cpr_make_pal(1, "endem"),
    "name is not a string"
  )
  expect_error(
    cpr_make_pal("mishler2014", 2),
    "type is not a string"
  )
  expect_error(
    cpr_make_pal("mishler2014", "foobar"),
    "'type' may only include 'endem' or 'signif'"
  )
  expect_error(
    cpr_make_pal("foobar", "endem"),
    "'name' may only include 'mishler2014', 'canaper1', 'canaper2', 'canaper3', or 'canaper4'" # nolint
  )
  expect_error(
    cpr_make_pal("canaper4", "signif"),
    "No palette available with that name and type"
  )
})

test_that("Output is formatted as expected", {
  expect_equal(
    cpr_make_pal("mishler2014", "endem"),
    mishler_endem_cols
  )
  expect_equal(
    cpr_make_pal("canaper1", "endem"),
    cpr_endem_cols
  )
  expect_equal(
    cpr_make_pal("canaper2", "endem"),
    cpr_endem_cols_2
  )
  expect_equal(
    cpr_make_pal("canaper3", "endem"),
    cpr_endem_cols_3
  )
  expect_equal(
    cpr_make_pal("canaper4", "endem"),
    cpr_endem_cols_4
  )
  expect_equal(
    cpr_make_pal("mishler2014", "signif"),
    mishler_signif_cols
  )
  expect_equal(
    cpr_make_pal("canaper1", "signif"),
    cpr_signif_cols
  )
  expect_equal(
    cpr_make_pal("canaper2", "signif"),
    cpr_signif_cols_2
  )
})
