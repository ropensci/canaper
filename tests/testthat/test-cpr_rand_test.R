test_that("Input is valid", {
   library(picante)
   data(phylocom)
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
})
