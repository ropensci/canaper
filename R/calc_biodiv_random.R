#' Calculate diversity metrics for a single random community
#'
#' For description of metrics available, see \code{\link{cpr_rand_test}()}
#'
#' @srrstats {G2.0a, G2.1a, G2.3b} Documents expectations on lengths, types of vector
#'   inputs, case-sensitivity
#' @param comm Dataframe or matrix; input community matrix with communities
#'   (sites) as rows and species as columns, including row names and column
#'   names.
#' @param phy List of class `phylo`; input phylogeny with total branch length
#'   scaled to 1
#' @param phy_alt List of class `phylo`; alternative phylogeny where all
#'   branches are of equal length, scaled to 1
#' @param n_iterations Numeric vector of length 1; Number of iterations to use
#'   when shuffling random community
#' @param thin Numeric vector of length 1; thinning parameter used by some
#'   null models in `vegan` (e.g., `quasiswap`).
#' @param metrics Character vector; names of metrics to calculate. May include
#'   one or more of: `pd`, `rpd`, `pe`, `rpe` (case-sensitive).
#'
#' @return List of vectors. Each vector is a biodiversity metric measured on the
#'   random community, in the same order as the rows in the input community.
#'   Names of the list correspond to `metrics`.
#'
#' @examples
#' phy <- biod_example$phy
#' comm <- biod_example$comm
#' phy_alt <- phy
#' phy_alt$edge.length <- rep(x = 1, times = length(phy_alt$edge.length))
#' phy_alt$edge.length <- phy_alt$edge.length / sum(phy_alt$edge.length)
#' phy$edge.length <- phy$edge.length / sum(phy$edge.length)
#' calc_biodiv_random(comm, phy, phy_alt, "tswap", 1000, metrics = c("pd", "pe", "pd_alt"))
#'
#' @srrstats {G1.4, G1.4a} uses roxygen
#'
#' @noRd
calc_biodiv_random <- function(comm, phy, phy_alt,
															 null_model,
															 n_iterations = 1, thin = 1,
															 metrics = c("pd", "rpd", "pe", "rpe", "pd_alt", "pe_alt")) {
	# Check input ----
	#' @srrstats {G2.1, G2.6} Check input types and lengths
	#' @srrstats {G2.0, G2.2} Assert input length is 1 as needed
	#' @srrstats {G2.3, G2.3a} Check univariate char input
	# - comm
	assertthat::assert_that(inherits(comm, "data.frame") | inherits(comm, "matrix"),
													msg = "'comm' must be of class 'data.frame' or 'matrix'")
	# - phy
	assertthat::assert_that(
		is.list(phy) && inherits(phy, "phylo"),
		msg = "'phy' must be a list of class 'phylo'")
	# - phy_alt
	assertthat::assert_that(
		is.list(phy_alt) && inherits(phy_alt, "phylo"),
		msg = "'phy_alt' must be a list of class 'phylo'")
	# - null_model
	assertthat::assert_that(
		assertthat::is.string(null_model) | inherits(null_model, "commsim"),
		msg = "'null_model' must be a string (character vector of length 1) or an object of class 'commsim'"
	)
	if (isTRUE(assertthat::is.string(null_model))) {
		assertthat::assert_that(assertthat::not_empty(comm))
		assertthat::assert_that(assertthat::noNA(null_model))
	}
	# - n_iterations
	assertthat::assert_that(assertthat::is.number(n_iterations))
	assertthat::assert_that(assertthat::noNA(n_iterations))
	assertthat::assert_that(is.finite(n_iterations))
	n_iterations <- as.integer(n_iterations)
	assertthat::assert_that(n_iterations > 0, msg = "'n_iterations' must be > 0")
	# - thin
	assertthat::assert_that(assertthat::is.number(thin))
	assertthat::assert_that(assertthat::noNA(thin))
	assertthat::assert_that(is.finite(thin))
	# - metrics
	assertthat::assert_that(is.character(metrics))
	assertthat::assert_that(assertthat::noNA(metrics))
	assertthat::assert_that(
		isTRUE(all(metrics %in% c("pd", "rpd", "pe", "rpe", "pd_alt", "pe_alt"))),
		msg = "'metrics' may only include 'pd', 'rpd', 'pe', 'rpe', 'pd_alt', 'pe_alt'"
	)

	# - Make sure names match between community and tree
	assertthat::assert_that(isTRUE(
		all.equal(sort(phy$tip.label), sort(colnames(comm)))
	))

	assertthat::assert_that(isTRUE(
		all.equal(sort(phy_alt$tip.label), sort(colnames(comm)))
	))

	# - Make sure phylogeny has been re-scaled to total branch length of 1 for RPE or RFD
	if (any(metrics %in% c("rpe", "rpd"))) {
		assertthat::assert_that(
			isTRUE(all.equal(sum(phy$edge.length), 1)),
			msg = "phylogeny not rescaled to total length 1"
		)
	}
	if (any(metrics %in% c("rpe", "rpd"))) {
		assertthat::assert_that(
			isTRUE(all.equal(sum(phy_alt$edge.length), 1)),
			msg = "alternative phylogeny not rescaled to total length 1"
		)
	}

	# - Check that phy_alt is different from phy
	assertthat::assert_that(
		!isTRUE((all.equal(phy, phy_alt))),
		msg = "'phy' and 'phy_alt' should not be identical"
	)

	# Calculations ----

	# Convert comm to sparse matrix format for phyloregions
	comm_sparse <- phyloregion::dense2sparse(comm)

	# Generate random community
	random_comm <- cpr_rand_comm(comm, null_model = null_model, n_iterations = n_iterations, thin = thin)
	random_comm_sparse <- phyloregion::dense2sparse(random_comm)

	# Calculate statistics for random community
	# - set up null vectors first
	pd <- NULL
	pd_alt <- NULL
	rpd <- NULL
	pe <- NULL
	pe_alt <- NULL
	rpe <- NULL

	# - calculate selected metrics
	if ("pd" %in% metrics) pd <- phyloregion::PD(random_comm_sparse, phy)
	if ("pd_alt" %in% metrics) pd_alt <- phyloregion::PD(random_comm_sparse, phy_alt)
	# pd_alt is inferred by rpd
	if ("rpd" %in% metrics) {
		if (is.null(pd)) pd <- phyloregion::PD(random_comm_sparse, phy)
		if (is.null(pd_alt)) pd_alt <- phyloregion::PD(random_comm_sparse, phy_alt)
		rpd <- pd / pd_alt
	}
	# pe_alt is inferred by rpe
	if ("pe" %in% metrics) pe <- phyloregion::phylo_endemism(random_comm_sparse, phy, weighted = TRUE)
	if ("pe_alt" %in% metrics) pe_alt <- phyloregion::phylo_endemism(random_comm_sparse, phy_alt, weighted = TRUE)
	if ("rpe" %in% metrics) {
		if (is.null(pe)) pe <- phyloregion::phylo_endemism(random_comm_sparse, phy, weighted = TRUE)
		if (is.null(pe_alt)) pe_alt <- phyloregion::phylo_endemism(random_comm_sparse, phy_alt, weighted = TRUE)
		rpe <- pe / pe_alt
	}

	# Output non-NULL results
	# (note that pd_alt and pe_alt will be included if rpd or rpe were included in `metrics`)
	purrr::compact(
		list(
			pd = pd,
			pd_alt = pd_alt,
			rpd = rpd,
			pe = pe,
			pe_alt = pe_alt,
			rpe = rpe
		)
	)

}
