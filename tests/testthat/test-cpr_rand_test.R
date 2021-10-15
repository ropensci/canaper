# Make datasets for testing ----

# -data with numeric species names
comm_num_names <- biod_example$comm
colnames(comm_num_names) <- 1:dim(comm_num_names)[2]
phy_num_names <- biod_example$phy
phy_num_names$tip.label <- colnames(comm_num_names)

# -comm with duplicated species names
comm_dup_names <- biod_example$comm
colnames(comm_dup_names)[2] <- "sp1"

# -phy with duplicated species names
phy_dup_names <- biod_example$phy
phy_dup_names$tip.label[2] <- "sp1"

# -comm with non-numeric data
comm_non_numeric <- biod_example$comm
comm_non_numeric[1,1] <- "a"

# -comm with NA data
comm_with_na <- biod_example$comm
comm_with_na[1,1] <- NA

# -comm with non vector data
comm_with_nonvec <- data.frame(biod_example$comm)
attributes(comm_with_nonvec[,1]) <- list(bar = "foo")

# -comm with infinite data
comm_with_infinite <- biod_example$comm
comm_with_infinite[1,1] <- Inf

# -comm with negative data
comm_with_negative <- biod_example$comm
comm_with_negative[1,1] <- -10

# -presence-absence community
comm_pa <- apply(biod_example$comm, 2, function(x) ifelse(x > 0, 1, 0))

# -comm in tibble format
comm_tbl <- biod_example$comm %>%
   tibble::rownames_to_column("site") %>%
   tibble::as_tibble()

# -comm in tibble format, non-default site name
comm_tbl_2 <- biod_example$comm %>%
   tibble::rownames_to_column("sample") %>%
   tibble::as_tibble()

# -comm in matrix format
comm_mat <- as.matrix(biod_example$comm)

# - negative branch lengths
phy_neg <- biod_example$phy
phy_neg$edge.length[1] <- -1

# - column with all zeros
comm_one_zero_col <- biod_example$comm
comm_one_zero_col[,1] <- 0

# - row with all zeros
comm_one_zero_row <- biod_example$comm
comm_one_zero_row[1,] <- 0

# Make output for testing ----
# For testing that results are same regardless of input type
# (need to have same random seed each time)
set.seed(123)
res_from_df <- cpr_rand_test(biod_example$comm, biod_example$phy, null_model = "richness", n_reps = 10)
set.seed(123)
res_from_tbl <- cpr_rand_test(comm_tbl, biod_example$phy, null_model = "richness", n_reps = 10, tbl_out = FALSE)
set.seed(123)
res_from_mat <- cpr_rand_test(comm_mat, biod_example$phy, null_model = "richness", n_reps = 10)

# For testing output format
res_tbl_from_df <- cpr_rand_test(biod_example$comm, biod_example$phy, null_model = "richness", n_reps = 1, metrics = "pd", tbl_out = TRUE)
res_tbl_from_tbl <- cpr_rand_test(comm_tbl, biod_example$phy, null_model = "richness", n_reps = 1, metrics = "pd")
res_df_from_tbl <- cpr_rand_test(comm_tbl, biod_example$phy, null_model = "richness", n_reps = 1, metrics = "pd", tbl_out = FALSE)

# Run tests ----

#' @srrstats {G5.2, G5.2a, G5.2b} tests failure if input is not valid and checks warning messages
test_that("Input is valid", {
   expect_error(
      cpr_rand_test(biod_example$comm, biod_example$phy, n_reps = -10),
      "'n_reps' must be > 0"
   )
   expect_error(
      cpr_rand_test(biod_example$comm, biod_example$phy, n_iterations = -10, null_model = "independentswap"),
      "'n_iterations' must be > 0"
   )
   expect_error(
      cpr_rand_test(10, biod_example$phy, metrics = "pd"),
      "'comm' must be of class 'data\\.frame' or 'matrix'"
   )
   expect_error(
      cpr_rand_test(biod_example$comm, NA, metrics = "pd"),
      "'phy' must be a list of class 'phylo'"
   )
   expect_error(
      cpr_rand_test(biod_example$comm, biod_example$phy, metrics = "pg"),
      "'metrics' may only include 'pd', 'rpd', 'pe', or 'rpe'"
   )
   comm_small <- biod_example$comm[1:3,]
   comm_small[1,] <- 1
   comm_small[2,] <- 1
   comm_small[3,] <- 1
   expect_error(
      cpr_rand_test(comm_small, biod_example$phy),
      "'comm' must include at least 5 sites"
   )
   expect_error(
      cpr_rand_test(biod_example$comm, ape::keep.tip(biod_example$phy, c("sp1", "sp2"))),
      "'phy' and 'comm' must share at least 5 species in common"
   )
   expect_error(
      cpr_rand_test(as.matrix(comm_num_names), phy_num_names, metrics = "pd"),
      "Column names of 'comm' changed after conversion from matrix to dataframe\\. Do any column names start with a number"
   )
   expect_error(
      cpr_rand_test(comm_dup_names, biod_example$phy, metrics = "pd"),
      "'comm' must have unique column names"
   )
   expect_error(
      cpr_rand_test(comm_non_numeric, biod_example$phy, metrics = "pd"),
      "All columns of 'comm' must be numeric"
   )
   expect_error(
      cpr_rand_test(comm_with_na, biod_example$phy, metrics = "pd"),
      "No missing values allowed in 'comm'"
   )
   expect_error(
      cpr_rand_test(comm_with_na, biod_example$phy, metrics = "pd"),
      "No missing values allowed in 'comm'"
   )
   expect_error(
      cpr_rand_test(comm_with_negative, biod_example$phy, metrics = "pd"),
      "No negative values allowed in 'comm'"
   )
   #' @srrstats {G2.11} test for non-vector inputs
   expect_false(
      is.vector(comm_with_nonvec[,1])
   )
   expect_error(
      cpr_rand_test(comm_with_nonvec, biod_example$phy, metrics = "pd"),
      "All columns of 'comm' must be numeric"
   )
   #' @srrstats {G2.16} don't allow infinite values
   expect_error(
      cpr_rand_test(comm_with_infinite, biod_example$phy, metrics = "pd"),
      "No infinite values allowed in 'comm'"
   )
   expect_error(
      cpr_rand_test(comm_tbl, biod_example$phy, metrics = "pd", site_col = "sample"),
      "'site_col' must be one of the column names of 'comm'"
   )
   #' @srrstats {UL1.4} Check assumptions made with regard to input data
   expect_error(
      cpr_rand_test(biod_example$comm, phy_neg),
      "'phy' may not have negative branchlengths"
   )
   expect_error(
      cpr_rand_test(comm_one_zero_col, biod_example$phy),
      "Every species in 'comm' must occur in at least one site"
   )
   expect_error(
      cpr_rand_test(comm_one_zero_row, biod_example$phy),
      "Every site in 'comm' must have at least once species"
   )
})

test_that("Results are same regardless of input type", {
   expect_equal(res_from_mat, res_from_df)
   expect_equal(res_from_mat, res_from_tbl)
   expect_equal(res_from_df, res_from_tbl)
})

test_that("Results are same regardless of presence/absence or abundance input", {
   # pd
   set.seed(123)
   res_abun_pd <- cpr_rand_test(biod_example$comm, biod_example$phy, null_model = "richness", n_reps = 10, metrics = "pd")
   set.seed(123)
   res_pa_pd <- cpr_rand_test(comm_pa, biod_example$phy, null_model = "richness", n_reps = 10, metrics = "pd")
   expect_equal(res_abun_pd, res_pa_pd)
   # pe
   set.seed(123)
   res_abun_pe <- cpr_rand_test(biod_example$comm, biod_example$phy, null_model = "richness", n_reps = 10, metrics = "pe")
   set.seed(123)
   res_pa_pe <- cpr_rand_test(comm_pa, biod_example$phy, null_model = "richness", n_reps = 10, metrics = "pe")
   expect_equal(res_abun_pe, res_pa_pe)
   # rpd
   set.seed(123)
   res_abun_rpd <- cpr_rand_test(biod_example$comm, biod_example$phy, null_model = "richness", n_reps = 10, metrics = "rpd")
   set.seed(123)
   res_pa_rpd <- cpr_rand_test(comm_pa, biod_example$phy, null_model = "richness", n_reps = 10, metrics = "rpd")
   expect_equal(res_abun_rpd, res_pa_rpd)
   # rpe
   set.seed(123)
   res_abun_rpe <- cpr_rand_test(biod_example$comm, biod_example$phy, null_model = "richness", n_reps = 10, metrics = "rpe")
   set.seed(123)
   res_pa_rpe <- cpr_rand_test(comm_pa, biod_example$phy, null_model = "richness", n_reps = 10, metrics = "rpe")
   expect_equal(res_abun_rpe, res_pa_rpe)
   # all
   set.seed(123)
   res_abun <- cpr_rand_test(biod_example$comm, biod_example$phy, null_model = "richness", n_reps = 10)
   set.seed(123)
   res_pa <- cpr_rand_test(comm_pa, biod_example$phy, null_model = "richness", n_reps = 10)
   expect_equal(res_abun, res_pa)
})

#' @srrstats {UL1.2} Check for default-looking column and rownames
test_that("Default column and rownames are detected", {
   # Make a dummy community dataframe and phylogeny
   set.seed(123)
   abuns <- runif(10*8, 0, 10) %>% as.integer()
   df_default_rows <- data.frame(
      sp1 = abuns[1:10],
      sp2 = abuns[11:20],
      sp3 = abuns[21:30],
      sp4 = abuns[31:40],
      sp5 = abuns[41:50],
      sp6 = abuns[51:60],
      sp7 = abuns[61:70],
      sp8 = abuns[71:80]
   )
   phy_default <- ape::keep.tip(biod_example$phy, paste0("sp", 1:8))
   expect_error(
      cpr_rand_test(df_default_rows, phy_default),
      "'comm' cannot have default row names \\(consecutive integers from 1 to the number of rows\\)"
   )
})

#' @srrstats {G5.4, G5.4b, G5.5} Implement correctness tests
# Make sure results from canaper match those of Biodiverse
# (for non-random results only)
test_that("Output is same as when calculated with Biodiverse", {
   # Calculate observed PD, PE, etc using biodiverse test data
   set.seed(123)
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
   # Use expect equal() with class() when *only* expecting a data.frame
   # (tibble inherits data.frame, tbl, and tbl_df)
   expect_equal(
      class(res_from_df),
      "data.frame"
   )
   expect_s3_class(
      res_tbl_from_df,
      "tbl_df")
   expect_s3_class(
      res_tbl_from_df,
      "tbl")
   expect_s3_class(
      res_tbl_from_tbl,
      "tbl_df")
   expect_s3_class(
      res_tbl_from_tbl,
      "tbl")
   expect_equal(
      class(res_df_from_tbl),
      "data.frame"
   )
   #' @srrstats {UL1.3} Make sure data from 'site_col' is carried through for tbl input
   res_tbl_from_tbl_2 <- cpr_rand_test(comm_tbl_2, biod_example$phy, null_model = "richness", n_reps = 1, metrics = "pd", site_col = "sample")
   expect_equal(
      comm_tbl_2$sample,
      res_tbl_from_tbl_2$sample
   )
   expect_equal(
      rownames(res_from_df),
      rownames(biod_example$comm)
   )
})
