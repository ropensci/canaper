test_that("counting higher values works", {
	expect_identical(count_higher(4, 1:10), 3L)
	expect_equal(count_higher(4, 1:10), 3L)
	expect_equal(count_higher(4, 1:10), 3)
	expect_equal(count_higher(4, c(1:10, NA)), 3)
	expect_equal(count_higher(4, c(1:10, NA), na.rm = FALSE), NaN)
	expect_equal(count_higher(4, c(1:10, NaN)), 3)
	expect_equal(count_higher(4, c(1:10, NaN), na.rm = FALSE), NaN)
	expect_equal(count_higher(4, vector("numeric", 0)), NaN)
})

test_that("counting lower values works", {
	expect_identical(count_lower(4, 1:10), 6L)
	expect_equal(count_lower(4, 1:10), 6L)
	expect_equal(count_lower(4, 1:10), 6)
	expect_equal(count_lower(4, c(1:10, NA)), 6)
	expect_equal(count_lower(4, c(1:10, NA), na.rm = FALSE), NaN)
	expect_equal(count_lower(4, c(1:10, NaN)), 6)
	expect_equal(count_lower(4, c(1:10, NaN), na.rm = FALSE), NaN)
	expect_equal(count_lower(4, vector("numeric", 0)), NaN)
})

# see https://stackoverflow.com/questions/2769510/numeric-comparison-difficulty-in-r
test_that("modified equalities work", {
	a <- 0.58
	b <- 0.08
	# This is the problem with normal `<`
	expect_true(
		(a - b) < 0.5
	)
	# Modified version fixes it
	expect_false(
		(a - b) %lesser% 0.5
	)
	expect_false(
		(a - b) %greater% 0.5
	)
	expect_false(
	  2.0 %greater% 2.0
	)
	expect_true(
		2.1 %greater% 2.0
	)
	expect_false(
		2.0 %greater% 2.1
	)
	expect_false(
		2.0 %lesser% 2.0
	)
	expect_false(
		2.1 %lesser% 2.0
	)
	expect_true(
		2.0 %lesser% 2.1
	)
	expect_true(
		2.0 %>=% 2.0
	)
	expect_true(
		2.1 %>=% 2.0
	)
	expect_false(
		2.0 %>=% 2.1
	)
	expect_true(
		2.0 %<=% 2.0
	)
	expect_false(
		2.1 %<=% 2.0
	)
	expect_true(
		2.0 %<=% 2.1
	)
}
)

test_that("Printing phylogenies works", {
	expect_output(
		print(biod_example$phy),
		"Phylogenetic tree with 31 tips and 30 internal nodes"
	)
})

test_that("Matching community and phylogeny data works", {
	comm_small <- biod_example$comm[,!colnames(biod_example$comm) %in% c("sp3", "sp5")]
	phy_small <- ape::drop.tip(biod_example$phy, c("sp1", "sp2"))
	dat_small_1 <- match_phylo_comm(biod_example$phy, comm_small)
	dat_small_2 <- match_phylo_comm(phy_small, biod_example$comm)
	dat_small_3 <- match_phylo_comm(phy_small, comm_small)
	dat_matched <- match_phylo_comm(biod_example$phy, biod_example$comm)
	dat_small_1_pic <- picante::match.phylo.comm(biod_example$phy, comm_small)
	dat_small_2_pic <- picante::match.phylo.comm(phy_small, biod_example$comm)
	dat_small_3_pic <- picante::match.phylo.comm(phy_small, comm_small)
	dat_matched_pic <- picante::match.phylo.comm(biod_example$phy, biod_example$comm)
	# Check matching against picante::match.phylo.comm()
	expect_equal(dat_small_1, dat_small_1_pic)
	expect_equal(dat_small_2, dat_small_2_pic)
	expect_equal(dat_small_3, dat_small_3_pic)
	expect_equal(dat_matched, dat_matched_pic)
	# Matching will put species into order matching tree
	expect_equal(
		colnames(dat_matched[["comm"]]),
		biod_example$phy$tip.label
	)
	# Matching doesn't change the tree
	expect_equal(
		dat_matched$phy,
		biod_example$phy
	)
	# Make sure order of community species doesn't matter for PD, PE
	expect_equal(
		phyloregion::PD(phyloregion::dense2sparse(dat_matched$comm), dat_matched$phy),
		phyloregion::PD(phyloregion::dense2sparse(biod_example$comm), biod_example$phy)
	)
	expect_equal(
		phyloregion::phylo_endemism(phyloregion::dense2sparse(dat_matched$comm), dat_matched$phy),
		phyloregion::phylo_endemism(phyloregion::dense2sparse(biod_example$comm), biod_example$phy)
	)
	# Warnings work
	expect_warning(
		match_phylo_comm(phy_small, biod_example$comm),
		"Dropping taxa from the community because they are not present in the phylogeny"
	)
	expect_warning(
		match_phylo_comm(biod_example$phy, comm_small),
		"Dropping tips from the tree because they are not present in the community data"
	)
})
