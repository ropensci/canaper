#' Run a randomization analysis for one or more biodiversity metrics
#'
#' The observed value of the biodiversity metric(s) will be calculated, then
#' compared against a set of random communities. Various statistics are calculated
#' from the comparison (see **Value** below).
#'
#' @srrstats {G1.3} defines terminology:
#' The biodiversity metrics available for analysis include:
#' - `pd`: Phylogenetic diversity (Faith 1992)
#' - `rpd`: Relative phylogenetic diversity (Mishler et al 2014)
#' - `pe`: Phylogenetic endemism (Rosauer et al 2009)
#' - `rpe`: Relative phylogenetic endemism (Mishler et al 2014)
#'
#' The default method for generating random communities is the independent swap
#' method of Gotelli (2000), which randomizes the community matrix while maintaining
#' species occurrence frequency and sample species richness.
#'
#' @param comm Dataframe or matrix; input community matrix with communities as rows
#' and species as columns, including row names and column names.
#' @param phy List of class `phylo`; input phylogeny.
#' @param null_model Name of null model to use. Must choose from `frequency`, `richness`,
#' `independentswap`, or `trialswap.` For details, see [picante::randomizeMatrix()].
#' @param n_reps Number of random communities to replicate.
#' @param n_iterations Number of iterations to use when swapping occurrences to
#' generate each random community; only used if `null_model` is 'independentswap'
#' or 'trialswap'.
#' @param metrics Character vector; names of biodiversity metrics to calculate.
#' May include one or more of: `pd`, `rpd`, `pe`, `rpe`.
#'
#' @srrstats {G1.3} defines terminology:
#' @return Dataframe. For each of the biodiversity metrics, the following 9 columns
#' will be produced:
#' - `*_obs`: Observed value
#' - `*_obs_c_lower`: Count of times observed value was lower than random values
#' - `*_obs_c_upper`: Count of times observed value was higher than random values
#' - `*_obs_p_lower`: Percentage of times observed value was lower than random values
#' - `*_obs_p_upper`: Percentage of times observed value was higher than random values
#' - `*_obs_q`: Count of the non-NA random values used for comparison
#' - `*_obs_z`: Standard effect size (z-score)
#' - `*_rand_mean`: Mean of the random values
#' - `*_rand_sd`: Standard deviation of the random values
#'
#' So if you included `pd` in `metrics`, the output columns would include `pd_obs`,
#' `pd_obs_c_lower`, etc...
#'
#' @srrstats {G1.0} Cites original refs:
#' @source Faith DP (1992) Conservation evaluation and phylogenetic diversity.
#'  Biological Conservation, 61:1–10. \doi{10.1016/0006-3207(92)91201-3}
#' @source Gotelli, N.J. (2000) Null Model Analysis of Species Co-Occurrence
#' Patterns. Ecology, 81: 2606-2621. \doi{10.1890/0012-9658(2000)081[2606:NMAOSC]2.0.CO;2}
#' @source Rosauer, D., Laffan, S.W., Crisp, M.D., Donnellan, S.C. and Cook, L.G. (2009)
#' Phylogenetic endemism: a new approach for identifying geographical concentrations of
#' evolutionary history. Molecular Ecology, 18: 4061-4072. \doi{10.1111/j.1365-294X.2009.04311.x}
#' @source Mishler, B., Knerr, N., González-Orozco, C. et al.  (2014) Phylogenetic measures
#' of biodiversity and neo- and paleo-endemism in Australian Acacia.
#' Nat Commun, 5: 4473. \doi{10.1038/ncomms5473}
#'
#' @examples
#' library(picante)
#' data(phylocom)
#' cpr_rand_test(phylocom$sample, phylocom$phy, metrics = "pd")
#'
#' @srrstats {G1.4} uses roxygen
#'
#' @export
cpr_rand_test <- function(comm, phy, null_model = "independentswap", n_reps = 100, n_iterations = 10000, metrics = c("pd", "rpd", "pe", "rpe")) {

	# Check input
	assertthat::assert_that(inherits(comm, "data.frame") | inherits(comm, "matrix"),
													msg = "'comm' must be of class 'data.frame' or 'matrix'")
	assertthat::assert_that(
		is.list(phy) && inherits(phy, "phylo"),
		msg = "'phy' must be a list of class 'phylo'")
	assertthat::assert_that(assertthat::is.string(null_model))
	assertthat::assert_that(
		isTRUE(null_model %in% c("frequency", "richness", "independentswap", "trialswap")),
		msg = "'null_model' must be one of 'frequency', 'richness', 'independentswap', or 'trialswap'"
	)
	assertthat::assert_that(assertthat::noNA(null_model))
	assertthat::assert_that(assertthat::is.number(n_reps))
	assertthat::assert_that(assertthat::noNA(n_reps))
	assertthat::assert_that(assertthat::is.number(n_iterations))
	assertthat::assert_that(assertthat::noNA(n_iterations))
	assertthat::assert_that(is.character(metrics))
	assertthat::assert_that(
		isTRUE(all(metrics %in% c("pd", "rpd", "pe", "rpe"))),
		msg = "'metrics' may only include 'pd', 'rpd', 'pe', or 'rpe'"
	)
	assertthat::assert_that(assertthat::noNA(metrics))

	# Verify that at least 5 species and sites are present
	# this could possibly be relaxed if https://github.com/skembel/picante/issues/26 gets fixed
	assertthat::assert_that(nrow(comm) > 4, msg = "'comm' must include at least 5 species")
	assertthat::assert_that(ncol(comm) > 4, msg = "'comm' must include at least 5 sites")
	assertthat::assert_that(ape::Ntip(phy) > 4, msg = "'phy' must include at least 5 species")
	assertthat::assert_that(assertthat::not_empty(comm))

	# Match tips of tree and column names of community data frame:
	# Use only taxa that are in common between phylogeny and community
	subsetted_data <- picante::match.phylo.comm(phy = phy, comm = comm)
	phy <- subsetted_data[["phy"]]
	comm <- subsetted_data[["comm"]]

	assertthat::assert_that(
		isTRUE(
			all.equal(
				sort(colnames(comm)),
				sort(phy$tip.label)
			)
		),
		msg = "Tip names don't match between community and phylogeny"
	)

	# Make alternative tree with equal branch lengths
	phy_alt <- phy
	phy_alt$edge.length <- rep(x = 1, times = length(phy_alt$edge.length))
	# rescale so total phy length is 1
	phy_alt$edge.length <- phy_alt$edge.length / sum(phy_alt$edge.length)
	# rescale original phy so total length is 1
	phy$edge.length <- phy$edge.length / sum(phy$edge.length)

	# Make sparse community df
	comm_sparse <- phyloregion::dense2sparse(comm)

	# Calculate biodiversity metrics for random communities
	# set up a progress bar
	pb <- progressr::progressor(steps = n_reps)

	random_vals <-
		furrr::future_map(
			1:n_reps,
			~ {
				pb()
				calc_biodiv_random(
					comm, phy, phy_alt,
					null_model = null_model,
					n_iterations = n_iterations,
					metrics = metrics
				)
			},
			.options = furrr::furrr_options(seed = TRUE)
		)

	# Calculate biodiversity metrics for observed community
	# - set up null vectors first
	ses_pd <- NULL
	ses_pd_alt <- NULL
	ses_rpd <- NULL
	ses_pe <- NULL
	ses_pe_alt <- NULL
	ses_rpe <- NULL

	# - calculate selected metrics
	if ("pd" %in% metrics) {
		pd_obs <- phyloregion::PD(comm_sparse, phy)
		ses_pd <- get_ses(random_vals, pd_obs, "pd")
	}

	if ("rpd" %in% metrics) {
		pd_alt_obs <- phyloregion::PD(comm_sparse, phy_alt)
		ses_pd_alt <- get_ses(random_vals, pd_alt_obs, "pd_alt")
		rpd_obs <- pd_obs / pd_alt_obs
		ses_rpd <- get_ses(random_vals, rpd_obs, "rpd")
	}

	if ("pe" %in% metrics) {
		pe_obs <- phyloregion::phylo_endemism(comm_sparse, phy, weighted = TRUE)
		ses_pe <- get_ses(random_vals, pe_obs, "pe")
	}

	if ("rpe" %in% metrics) {
		pe_alt_obs <- phyloregion::phylo_endemism(comm_sparse, phy_alt, weighted = TRUE)
		ses_pe_alt <- get_ses(random_vals, pe_alt_obs, "pe_alt")
		rpe_obs <- pe_obs / pe_alt_obs
		ses_rpe <- get_ses(random_vals, rpe_obs, "rpe")
	}

	# Combine results
	results <- dplyr::bind_cols(
		ses_pd,
		ses_pd_alt,
		ses_rpd,
		ses_pe,
		ses_pe_alt,
		ses_rpe
	)

	results <- dplyr::mutate(results, site = rownames(comm))

	tibble::column_to_rownames(results, "site")
}
