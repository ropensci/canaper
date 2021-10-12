library(picante)
data(phylocom)
phy <- phylocom$phy
comm <- phylocom$sample
subsetted_data <- picante::match.phylo.comm(phy = phy, comm = comm)
phy <- subsetted_data[["phy"]]
comm <- subsetted_data[["comm"]]
phy_alt <- phy
phy_alt$edge.length <- rep(x = 1, times = length(phy_alt$edge.length))
phy_alt$edge.length <- phy_alt$edge.length / sum(phy_alt$edge.length)
phy$edge.length <- phy$edge.length / sum(phy$edge.length)

#' @srrstats {G5.2, G5.2a, G5.2b} tests failure if input is not valid and checks warning messages
test_that("Input is valid", {
	expect_error(
		calc_biodiv_random(10, phy, phy_alt, "independentswap"),
		"'comm' must be of class 'data\\.frame' or 'matrix'"
	)
	expect_error(
		calc_biodiv_random(comm, NA, phy_alt, "independentswap"),
		"'phy' must be a list of class 'phylo'"
	)
	expect_error(
		calc_biodiv_random(comm, phy, phy_alt, "independentswap", metrics = "pg"),
		"'metrics' may only include 'pd', 'rpd', 'pe', 'rpe', 'pd_alt', 'pe_alt'"
	)
	expect_error(
		calc_biodiv_random(comm, phy, phy_alt, "indepswap", metrics = "pd"),
		"'null_model' must be one of 'frequency', 'richness', 'independentswap', or 'trialswap'"
	)
	expect_error(
		calc_biodiv_random(comm[1:3,1:3], phy, phy_alt, "independentswap"),
		"'comm' must include at least 5 species"
	)
	expect_error(
		calc_biodiv_random(comm, ape::keep.tip(phylocom$phy, c("sp1", "sp2")), phy_alt, "independentswap"),
		"'phy' must include at least 5 species"
	)
	expect_error(
		calc_biodiv_random(comm, phy, phy, "independentswap"),
		"'phy' and 'phy_alt' should not be identical"
	)
	expect_error(
		calc_biodiv_random(comm, subsetted_data[["phy"]], phy_alt, "independentswap"),
		"phylogeny not rescaled to total length 1"
	)
	expect_error(
		calc_biodiv_random(comm, phy, subsetted_data[["phy"]], "independentswap"),
		"alternative phylogeny not rescaled to total length 1"
	)
})

test_that("Selected metric shows up in output", {
	expect_named(calc_biodiv_random(comm, phy, phy_alt, "independentswap", metrics = "pd"), "pd")
	expect_named(calc_biodiv_random(comm, phy, phy_alt, "independentswap", metrics = "pe"), "pe")
	expect_named(calc_biodiv_random(comm, phy, phy_alt, "independentswap", metrics = "rpd"), c("pd", "pd_alt", "rpd"), ignore.order = TRUE)
	expect_named(calc_biodiv_random(comm, phy, phy_alt, "independentswap", metrics = "rpe"), c("pe", "pe_alt", "rpe"), ignore.order = TRUE)
	expect_named(
		calc_biodiv_random(comm, phy, phy_alt, "independentswap", metrics = c("rpd", "rpe")),
		c("pd", "pd_alt", "rpd", "pe", "pe_alt", "rpe"), ignore.order = TRUE
	)
	expect_named(
		calc_biodiv_random(comm, phy, phy_alt, "independentswap", metrics = c("pd", "pe")),
		c("pd", "pe"),
		ignore.order = TRUE
	)
	expect_named(
		calc_biodiv_random(comm, phy, phy_alt, "independentswap", metrics = c("pd", "pe", "pd_alt")),
		c("pd", "pe", "pd_alt"),
		ignore.order = TRUE
	)
})
