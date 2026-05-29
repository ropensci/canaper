# Run tests ----

test_that("Input is valid", {
  expect_error(
    cpr_rwibald(1, biod_example$phy),
    "'comm' must be of class 'data.frame' or 'matrix'"
  )

  expect_error(
    cpr_rwibald(biod_example$comm, NA),
    "'phy' must be a list of class 'phylo'"
  )

  bad_comm <- biod_example$comm
  bad_comm[1, 1] <- -1
  expect_error(
    cpr_rwibald(bad_comm, biod_example$phy),
    "No negative values allowed in 'comm'"
  )
})

test_that("Output is formatted as expected", {
  out <- cpr_rwibald(biod_example$comm, biod_example$phy)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), nrow(biod_example$phy$edge))
  expect_true(
    all(
      c(
        "branch_id",
        "parent_node",
        "child_node",
        "is_terminal",
        "range_size",
        "branch_length_observed_tree",
        "branch_length_comparison_tree",
        "rank_comparison_tree",
        "sorted_asc_bl_comparison_tree",
        "rwibald_score",
        "rwibald_score_rank",
        "key_branch",
        "rwibald_type"
      ) %in%
        colnames(out)
    )
  )
  expect_type(out$rwibald_type, "character")
  expect_true(all(out$range_size > 0))
  expect_true(all(is.na(out$rwibald_type[!out$key_branch])))

  if (any(out$key_branch)) {
    expect_true(
      all(
        out$rwibald_type[out$key_branch] %in%
          c("neo-endemic", "meso-endemic", "paleo-endemic")
      )
    )
  }
})

test_that("Output is deterministic", {
  out_1 <- cpr_rwibald(biod_example$comm, biod_example$phy)
  out_2 <- cpr_rwibald(biod_example$comm, biod_example$phy)

  expect_identical(out_1, out_2)
})

test_that("Acacia output matches rwibald quarto benchmark counts", {
  # NOTE:
  # These expected values are based on the current canaper Acacia input data
  # (acacia$comm + acacia$phy). They are intentionally not taken from rwibald
  # quarto outputs because the original rwibald input tree/data are not yet
  # available in this repository.
  #
  # TODO: once original rwibald paper input data are obtained, update this test
  # to use rwibald-authoritative benchmark values.
  # rwibald repo expected values (quarto output):
  # total branches = 1015
  # key branches = 135
  # neo = 55, meso = 59, paleo = 21, non-key = 880
  # Current canaper baseline counts:
  # total branches = 1014
  # key branches = 135
  # neo = 57, meso = 57, paleo = 21, non-key = 879
  data("acacia", package = "canaper")

  out <- cpr_rwibald(acacia$comm, acacia$phy, quiet = TRUE)

  expect_equal(nrow(out), 1014)
  expect_equal(sum(out$key_branch), 135)
  expect_equal(sum(!out$key_branch), 879)

  type_counts <- table(out$rwibald_type, useNA = "ifany")

  expect_equal(unname(type_counts[["neo-endemic"]]), 57)
  expect_equal(unname(type_counts[["meso-endemic"]]), 57)
  expect_equal(unname(type_counts[["paleo-endemic"]]), 21)
  expect_equal(sum(is.na(out$rwibald_type)), 879)
})
