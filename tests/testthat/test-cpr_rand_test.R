library(picante)
# load phylocom data
data(phylocom)

# make data with numeric species names
comm_num_names <- phylocom$sample
colnames(comm_num_names) <- 1:dim(comm_num_names)[2]
phy_num_names <- phylocom$phy
phy_num_names$tip.label <- colnames(comm_num_names)

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
      cpr_rand_test(phylocom$sample[1:3,], phylocom$phy),
      "'comm' must include at least 5 sites"
   )
   expect_error(
      cpr_rand_test(phylocom$sample[,1:3], phylocom$phy),
      "'phy' and 'comm' must share at least 5 species in common"
   )
   expect_error(
      cpr_rand_test(phylocom$sample, ape::keep.tip(phylocom$phy, c("sp1", "sp2"))),
      "'phy' and 'comm' must share at least 5 species in common"
   )
   expect_error(
      cpr_rand_test(comm_num_names, phy_num_names, metrics = "pd"),
      "Column names of 'comm' changed after conversion from matrix to dataframe\\. Do any column names start with a number"
   )
})

test_that("Output is formatted as expected", {
   expect_s3_class(
      cpr_rand_test(phylocom$sample, phylocom$phy, metrics = "pd"),
      "data.frame")
})
