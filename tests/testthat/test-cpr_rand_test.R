library(picante)
# load phylocom data
data(phylocom)

# make data with numeric species names
comm_num_names <- phylocom$sample
colnames(comm_num_names) <- 1:dim(comm_num_names)[2]
phy_num_names <- phylocom$phy
phy_num_names$tip.label <- colnames(comm_num_names)

# make comm with duplicated species names
comm_dup_names <- phylocom$sample
colnames(comm_dup_names)[2] <- "sp1"

# make phy with duplicated species names
phy_dup_names <- phylocom$phy
phy_dup_names$tip.label[2] <- "sp1"

# make comm with missing data
comm_dup_names <- phylocom$sample
colnames(comm_dup_names)[2] <- "sp1"

# make comm with non-numeric data
comm_non_numeric <- phylocom$sample
comm_non_numeric[1,1] <- "a"

# make comm with NA data
comm_with_na <- phylocom$sample
comm_with_na[1,1] <- NA

# make comm with non vector data
comm_with_nonvec <- data.frame(phylocom$sample)
attributes(comm_with_nonvec[,1]) <- list(bar = "foo")

# make comm with infinite data
comm_with_infinite <- data.frame(phylocom$sample)
comm_with_infinite[1,1] <- Inf

#' @srrstats {G5.2, G5.2a, G5.2b} tests failure if input is not valid and checks warning messages
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
   expect_error(
      cpr_rand_test(comm_dup_names, phylocom$phy, metrics = "pd"),
      "Column names of 'comm' changed after conversion from matrix to dataframe\\. Do any column names start with a number"
   )
   expect_error(
      cpr_rand_test(comm_non_numeric, phylocom$phy, metrics = "pd"),
      "All columns of 'comm' must be numeric"
   )
   expect_error(
      cpr_rand_test(comm_with_na, phylocom$phy, metrics = "pd"),
      "No missing values allowed in 'comm'"
   )
   expect_error(
      cpr_rand_test(phylocom$sample, phy_dup_names, metrics = "pd"),
      "All tip labels in 'phy' must be unique"
   )
   #' @srrstats {G2.11} test for non-vector inputs
   expect_false(
      is.vector(comm_with_nonvec[,1])
   )
   expect_error(
      cpr_rand_test(comm_with_nonvec, phylocom$phy, metrics = "pd"),
      "All columns of 'comm' must be numeric"
   )
   #' @srrstats {G2.16} don't allow infinite values
   expect_error(
      cpr_rand_test(comm_with_infinite, phylocom$phy, metrics = "pd"),
      "No infinite values allowed in 'comm'"
   )
})

test_that("Output is formatted as expected", {
   expect_s3_class(
      cpr_rand_test(phylocom$sample, phylocom$phy, metrics = "pd"),
      "data.frame")
})
