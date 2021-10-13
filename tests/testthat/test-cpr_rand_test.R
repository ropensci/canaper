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
comm_with_infinite <- phylocom$sample
comm_with_infinite[1,1] <- Inf

# make comm with negative data
comm_with_negative <- phylocom$sample
comm_with_negative[1,1] <- -10

# make presence-absence community
comm_pa <- apply(phylocom$sample, 2, function(x) ifelse(x > 0, 1, 0))

#' @srrstats {G5.2, G5.2a, G5.2b} tests failure if input is not valid and checks warning messages
test_that("Input is valid", {
   expect_error(
      cpr_rand_test(phylocom$sample, phylocom$phy, n_reps = -10),
      "'n_reps' must be > 0"
   )
   expect_error(
      cpr_rand_test(phylocom$sample, phylocom$phy, n_iterations = -10, null_model = "independentswap"),
      "'n_iterations' must be > 0"
   )
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
      cpr_rand_test(comm_with_na, phylocom$phy, metrics = "pd"),
      "No missing values allowed in 'comm'"
   )
   expect_error(
      cpr_rand_test(comm_with_negative, phylocom$phy, metrics = "pd"),
      "No negative values allowed in 'comm'"
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

test_that("Results are same regardless of presence/absence or abundance input", {
   # pd
   set.seed(123)
   res_abun_pd <- cpr_rand_test(phylocom$sample, phylocom$phy, metrics = "pd")
   set.seed(123)
   res_pa_pd <- cpr_rand_test(comm_pa, phylocom$phy, metrics = "pd")
   expect_equal(res_abun_pd, res_pa_pd)
   # pe
   set.seed(123)
   res_abun_pe <- cpr_rand_test(phylocom$sample, phylocom$phy, metrics = "pe")
   set.seed(123)
   res_pa_pe <- cpr_rand_test(comm_pa, phylocom$phy, metrics = "pe")
   expect_equal(res_abun_pe, res_pa_pe)
   # rpd
   set.seed(123)
   res_abun_rpd <- cpr_rand_test(phylocom$sample, phylocom$phy, metrics = "rpd")
   set.seed(123)
   res_pa_rpd <- cpr_rand_test(comm_pa, phylocom$phy, metrics = "rpd")
   expect_equal(res_abun_rpd, res_pa_rpd)
   # rpe
   set.seed(123)
   res_abun_rpe <- cpr_rand_test(phylocom$sample, phylocom$phy, metrics = "rpe")
   set.seed(123)
   res_pa_rpe <- cpr_rand_test(comm_pa, phylocom$phy, metrics = "rpe")
   expect_equal(res_abun_rpe, res_pa_rpe)
   # all
   set.seed(123)
   res_abun <- cpr_rand_test(phylocom$sample, phylocom$phy)
   set.seed(123)
   res_pa <- cpr_rand_test(comm_pa, phylocom$phy)
   expect_equal(res_abun, res_pa)
})

#' @srrstats {G5.4, G5.4b} Implement correctness tests
# Make sure results from canaper match those of Biodiverse
# (for non-random results only)
test_that("Output is same as when calculated with Biodiverse", {
   # Calculate observed PD, PE, etc using biodiverse test data
   res_compare <- cpr_rand_test(biod_example$comm, biod_example$phy, n_reps = 1, null_model = "richness") %>%
      tibble::rownames_to_column("site") %>%
      tibble::as_tibble() %>%
      dplyr::select(site, matches("_obs$")) %>%
      # Join on independently calculated results from Biodiverse
      dplyr::left_join(biod_results, by = "site") %>%
      # Make sure no values are NA
      assertr::assert(assertr::not_na, dplyr::everything())
   expect_equal(res_compare$pd_obs, res_compare$pd_biodiv)
   expect_equal(res_compare$pd_alt_obs, res_compare$pd_alt_biodiv)
   expect_equal(res_compare$rpd_obs, res_compare$rpd_biodiv)
   expect_equal(res_compare$pe_obs, res_compare$pe_biodiv)
   expect_equal(res_compare$pe_alt_obs, res_compare$pe_alt_biodiv)
   expect_equal(res_compare$rpe_obs, res_compare$rpe_biodiv)
})

test_that("Output is formatted as expected", {
   expect_s3_class(
      cpr_rand_test(phylocom$sample, phylocom$phy, metrics = "pd"),
      "data.frame")
   #' @srrstats {G5.3} check that output has no missing values
   expect_true(
      assertr::assert(
         cpr_rand_test(phylocom$sample, phylocom$phy), assertr::not_na, dplyr::everything(),
         success_fun = assertr::success_logical, error_fun = assertr::error_logical
      )
   )
})
