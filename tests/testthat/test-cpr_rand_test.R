library(picante)
data(phylocom)

test_that("Input is valid", {
   expect_error(
      cpr_rand_test(10, phylocom$phy, metrics = "pd"),
      "'comm' must be of class 'data\\.frame' or 'matrix'"
   )
   expect_error(
      cpr_rand_test(phylocom$sample, NA, metrics = "pd"),
      "'phy' must be a list of class 'phylo'"
   )
   expect_error(
      cpr_rand_test(phylocom$sample, phylocom$phy, metrics = "pg"),
      "'metrics' may only include 'pd', 'rpd', 'pe', or 'rpe'"
   )
   expect_error(
      cpr_rand_test(phylocom$sample[1:3,1:3], phylocom$phy),
      "'comm' must include at least 5 species"
   )
   expect_error(
      cpr_rand_test(phylocom$sample, ape::keep.tip(phylocom$phy, c("sp1", "sp2"))),
      "'phy' must include at least 5 species"
   )
})

test_that("Output is formatted as expected", {
   expect_s3_class(
      cpr_rand_test(phylocom$sample, phylocom$phy, metrics = "pd"),
      "data.frame")
})
