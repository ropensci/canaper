#' Calculate diversity metrics for a single random community
#'
#' The independent swap method of Gotelli (2000) is used, which randomizes
#' the community matrix while maintaining species occurrence frequency and
#' sample species richness.
#'
#' For description of metrics available, see run_ses_analysis()
#'
#' @param comm Input community matrix in data.frame format (communities as rows,
#' species as columns, with row names and column names)
#' @param phy Input phylogeny with total branch length scaled to 1
#' @param phy_alt Alternative phylogeny where all branches are of equal length, scaled to 1
#' @param n_iterations Number of iterations to use when shuffling random community
#' @param metrics Character vector; names of metrics to calculate
#'
#' @return List of vectors. Each vector is a biodiversity metric measured on the
#' random community, in the same order as the rows in the input community.
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
#' calc_biodiv_random(comm, phy, phy_alt, "independentswap", metrics = "pd")
#' }
#' @keywords internal
calc_biodiv_random <- function(comm, phy, phy_alt,
															 null_model = c("frequency", "richness", "independentswap", "trialswap"),
															 n_iterations = 1000, metrics) {
	assertthat::assert_that(assertthat::is.string(null_model))
	assertthat::assert_that(
		null_model %in% c("frequency", "richness", "independentswap", "trialswap"),
		msg = "'null_model' must be one of 'frequency', 'richness', 'independentswap', or 'trialswap'"
	)

	# Make sure names match between community and tree
	assertthat::assert_that(isTRUE(
		all.equal(sort(phy$tip.label), sort(colnames(comm)))
	))

	assertthat::assert_that(isTRUE(
		all.equal(sort(phy_alt$tip.label), sort(colnames(comm)))
	))

	# Make sure phylogeny has been rescaled to total branch length of 1 for RPE or RFD
	if (any(metrics %in% c("rpe", "rpd"))) assertthat::assert_that(isTRUE(all.equal(sum(phy$edge.length), 1)))
	if (any(metrics %in% c("rpe", "rpd"))) assertthat::assert_that(isTRUE(all.equal(sum(phy_alt$edge.length), 1)))

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
	if ("rpd" %in% metrics) {
		pd_alt <- phyloregion::PD(random_comm_sparse, phy_alt)
		rpd <- pd / pd_alt
	}
	if ("pe" %in% metrics) pe <- phyloregion::phylo_endemism(random_comm_sparse, phy, weighted = TRUE)
	if ("rpe" %in% metrics) {
		pe_alt <- phyloregion::phylo_endemism(random_comm_sparse, phy_alt, weighted = TRUE)
		rpe <- pe / pe_alt
	}

	# Output results, only keep non-NULL results
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
