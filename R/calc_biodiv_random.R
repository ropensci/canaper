#' Calculate diversity metrics for a single random community
#'
#' For description of metrics available, see \code{\link{cpr_rand_test}()}
#'
#' @param comm Dataframe or matrix; input community matrix with communities (sites) as rows
#' and species as columns, including row names and column names.
#' @param phy Input phylogeny with total branch length scaled to 1
#' @param phy_alt Alternative phylogeny where all branches are of equal length, scaled to 1
#' @param n_iterations Number of iterations to use when shuffling random community
#' @param metrics Character vector; names of metrics to calculate
#'
#' @return List of vectors. Each vector is a biodiversity metric measured on the
#' random community, in the same order as the rows in the input community. Names
#' of the list correspond to `metrics`.
#'
#' @examples
#' \dontrun{
#' library(picante)
#' data(phylocom)
#' phy <- phylocom$phy
#' comm <- phylocom$sample
#' subsetted_data <- picante::match.phylo.comm(phy = phy, comm = comm)
#' phy <- subsetted_data[["phy"]]
#' comm <- subsetted_data[["comm"]]
#' phy_alt <- phy
#' phy_alt$edge.length <- rep(x = 1, times = length(phy_alt$edge.length))
#' phy_alt$edge.length <- phy_alt$edge.length / sum(phy_alt$edge.length)
#' phy$edge.length <- phy$edge.length / sum(phy$edge.length)
#' calc_biodiv_random(comm, phy, phy_alt, "independentswap", metrics = c("pd", "pe", "pd_alt"))
#' }
#' @keywords internal
calc_biodiv_random <- function(comm, phy, phy_alt,
															 null_model = c("frequency", "richness", "independentswap", "trialswap"),
															 n_iterations = 1000, metrics = c("pd", "rpd", "pe", "rpe")) {
	# Check input
	assertthat::assert_that(inherits(comm, "data.frame") | inherits(comm, "matrix"),
													msg = "'comm' must be of class 'data.frame' or 'matrix'")
	assertthat::assert_that(
		is.list(phy) && inherits(phy, "phylo"),
		msg = "'phy' must be a list of class 'phylo'")
	assertthat::assert_that(
		is.list(phy_alt) && inherits(phy_alt, "phylo"),
		msg = "'phy_alt' must be a list of class 'phylo'")
	assertthat::assert_that(assertthat::is.string(null_model))
	assertthat::assert_that(assertthat::noNA(null_model))
	assertthat::assert_that(
		isTRUE(null_model %in% c("frequency", "richness", "independentswap", "trialswap")),
		msg = "'null_model' must be one of 'frequency', 'richness', 'independentswap', or 'trialswap'"
	)
	assertthat::assert_that(assertthat::is.number(n_iterations))
	assertthat::assert_that(assertthat::noNA(n_iterations))
	assertthat::assert_that(is.character(metrics))
	assertthat::assert_that(
		isTRUE(all(metrics %in% c("pd", "pd_alt", "rpd", "pe", "pe_alt", "rpe"))),
		msg = "'metrics' may only include 'pd', 'rpd', 'pe', or 'rpe'"
	)
	assertthat::assert_that(assertthat::noNA(metrics))

	# Verify that at least 5 species and sites are present
	# this could possibly be relaxed if https://github.com/skembel/picante/issues/26 gets fixed
	assertthat::assert_that(nrow(comm) > 4, msg = "'comm' must include at least 5 species")
	assertthat::assert_that(ncol(comm) > 4, msg = "'comm' must include at least 5 sites")
	assertthat::assert_that(ape::Ntip(phy) > 4, msg = "'phy' must include at least 5 species")
	assertthat::assert_that(assertthat::not_empty(comm))

	# Make sure names match between community and tree
	assertthat::assert_that(isTRUE(
		all.equal(sort(phy$tip.label), sort(colnames(comm)))
	))

	assertthat::assert_that(isTRUE(
		all.equal(sort(phy_alt$tip.label), sort(colnames(comm)))
	))

	# Make sure phylogeny has been re-scaled to total branch length of 1 for RPE or RFD
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

	# Check that phy_alt is different from phy
	assertthat::assert_that(
		isTRUE(!(all.equal(phy, phy_alt))),
		msg = "'phy' and 'phy_alt' should not be identical"
	)

	# Convert comm to sparse matrix format for phyloregions
	comm_sparse <- phyloregion::dense2sparse(comm)

	# Generate random community
	random_comm <- picante::randomizeMatrix(comm, null.model = null_model, iterations = n_iterations)
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
	# pd_alt is inferred by rpd
	if ("rpd" %in% metrics) {
		pd_alt <- phyloregion::PD(random_comm_sparse, phy_alt)
		rpd <- pd / pd_alt
	}
	# pe_alt is inferred by rpe
	if ("pe" %in% metrics) pe <- phyloregion::phylo_endemism(random_comm_sparse, phy, weighted = TRUE)
	if ("rpe" %in% metrics) {
		pe_alt <- phyloregion::phylo_endemism(random_comm_sparse, phy_alt, weighted = TRUE)
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
