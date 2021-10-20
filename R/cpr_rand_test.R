#' Run a randomization analysis for one or more biodiversity metrics
#'
#' The observed value of the biodiversity metric(s) will be calculated, then
#' compared against a set of random communities. Various statistics are calculated
#' from the comparison (see **Value** below).
#'
#' The biodiversity metrics available for analysis include:
#' - `pd`: Phylogenetic diversity (Faith 1992)
#' - `rpd`: Relative phylogenetic diversity (Mishler et al 2014)
#' - `pe`: Phylogenetic endemism (Rosauer et al 2009)
#' - `rpe`: Relative phylogenetic endemism (Mishler et al 2014)
#'
#' The default method for generating random communities is the independent swap
#' method of Gotelli (2000) (`independentswap`), which randomizes the community
#' matrix while maintaining species occurrence frequency and sample species
#' richness.
#'
#' Note that the `trialswap` method has been found to produce results that
#' differ strongly from `independentswap` or the swapping algorithm of
#' [Biodiverse](https://github.com/shawnlaffan/biodiverse) (`rand_structured`),
#' and **is not recommended.**
#'
#' A minimum of 5 species and sites are required as input; fewer than that may
#' cause the default method for generating random communities (independent swap)
#' to enter an infinite loop. Besides, inferences on very small numbers of
#' species and/or sites is not recommended generally.
#'
#' The following rules apply to `comm` input:
#' - If dataframe or matrix, must include row names (site names) and column names (species names).
#' - If tibble, a single column (default, `site`) must be included with site names, and other columns must correspond to species names.
#' - Column names cannot start with a number and must be unique.
#' - Row names (site names) must be unique.
#' - Values (other than site names) should only include integers >= 0; non-integer input will be converted to integer.
#'
#' The results are identical regardless of whether the input for `comm` is
#' abundance or presence-absence data (i.e., abundance weighting is not used).
#'
#' @srrstats {G2.0a, G2.1a, G2.3b} Documents expectations on lengths, types of vector
#'   inputs, case-sensitivity
#' @srrstats {G2.7, UL1.0} accept dataframe or matrix
#' @srrstats {UL4.3a} If `tbl_out` is `TRUE`, restricts number of rows/columns printed to screen
#' @param comm Dataframe, tibble, or matrix; input community data with
#'   sites (communities) as rows and species as columns. Values of each cell are
#'   the presence/absence (0 or 1) or number of individuals (abundance) of each
#'   species in each site.
#' @param phy List of class `phylo`; input phylogeny.
#' @param null_model Character vector of length 1; name of null model to use.
#'   Must choose from `frequency`, `richness`, `independentswap`, or
#'   `trialswap` (case-sensitive). For details, see [picante::randomizeMatrix()].
#' @param n_reps Numeric vector of length 1; number of random communities to
#'   replicate.
#' @param n_iterations Numeric vector of length 1; number of iterations to use
#'   when swapping occurrences to generate each random community; only used if
#'   `null_model` is `independentswap` or `trialswap`.
#' @param metrics Character vector; names of biodiversity metrics to calculate.
#'   May include one or more of: `pd`, `rpd`, `pe`, `rpe` (case-sensitive).
#' @param site_col Character vector of length 1; name of column in `comm` that
#' contains the site names; only used if `comm` is a tibble (object of class
#' `tbl_df`).
#' @param tbl_out Logical vector of length 1; should the output be returned as
#' a tibble? If `FALSE`, will return a dataframe. Defaults to `TRUE` if `comm` is
#' a tibble.
#'
#' @srrstats {G1.3} defines terminology (also in 'details')
#' @srrstats {UL3.4} output includes variances in random values
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
#' cpr_rand_test(phylocom$sample, phylocom$phy, null_model = "trialswap", metrics = "pd")
#'
#' @srrstats {G1.4} uses roxygen
#'
#' @export
cpr_rand_test <- function(
	comm, phy, null_model = "independentswap",
	n_reps = 100, n_iterations = 10000,
	metrics = c("pd", "rpd", "pe", "rpe"),
	site_col = "site", tbl_out = tibble::is_tibble(comm)) {

	# capture state of n_reps and n_iterations for testing if different from default later
	n_reps_input <- n_reps
	n_iterations_input <- n_iterations

	# Check input: `null_model`, `n_reps`, `n_iterations`, `metrics` ----
	#' @srrstats {G2.0, G2.2, G2.1, G2.3, G2.3a, G2.6, G2.13, G2.14, G2.14a, G2.15, G2.16}
	#' check input types and lengths, missingness, undefined values, values of univariate char input
	# null_model
	assertthat::assert_that(assertthat::is.string(null_model))
	assertthat::assert_that(assertthat::noNA(null_model))
	assertthat::assert_that(
		isTRUE(null_model %in% c("frequency", "richness", "independentswap", "trialswap")),
		msg = "'null_model' must be one of 'frequency', 'richness', 'independentswap', or 'trialswap'"
	)
	# n_reps
	assertthat::assert_that(assertthat::is.number(n_reps))
	#' @srrstats {G2.4a} Convert to integer
	n_reps <- as.integer(n_reps)
	assertthat::assert_that(is.integer(n_reps))
	assertthat::assert_that(assertthat::noNA(n_reps))
	assertthat::assert_that(!is.infinite(n_reps))
	assertthat::assert_that(n_reps > 0, msg = "'n_reps' must be > 0")
	# metrics
	assertthat::assert_that(is.character(metrics))
	assertthat::assert_that(
		isTRUE(all(metrics %in% c("pd", "rpd", "pe", "rpe"))),
		msg = "'metrics' may only include 'pd', 'rpd', 'pe', or 'rpe'"
	)
	assertthat::assert_that(assertthat::noNA(metrics))
	# n_iterations (only needed for `independentswap`, `trialswap`)
	if (null_model %in% c("independentswap", "trialswap")) {
		assertthat::assert_that(assertthat::is.number(n_iterations))
		#' @srrstats {G2.4a} Convert to integer
		n_iterations <- as.integer(n_iterations)
		assertthat::assert_that(is.integer(n_iterations))
		assertthat::assert_that(assertthat::noNA(n_iterations))
		assertthat::assert_that(!is.infinite(n_iterations))
		assertthat::assert_that(n_iterations > 0, msg = "'n_iterations' must be > 0")
	} else {n_iterations <- NULL}
	# tbl_out
	assertthat::assert_that(assertthat::is.flag(tbl_out))

	# Check input: `comm` ----
	#' @srrstats {UL1.1} assert that all input data is of the expected form
	assertthat::assert_that(inherits(comm, "data.frame") | inherits(comm, "matrix"),
													msg = "'comm' must be of class 'data.frame' or 'matrix'")
	#' @srrstats {G2.8} Convert tibble to dataframe
	if (tibble::is_tibble(comm)) {
		assertthat::assert_that(assertthat::is.string(site_col))
		assertthat::assert_that(assertthat::noNA(site_col))
		assertthat::assert_that(
			isTRUE(site_col %in% colnames(comm)),
			msg = "'site_col' must be one of the column names of 'comm'")
		assertthat::assert_that(
			isTRUE(all(assertr::is_uniq(comm[[site_col]]))),
			msg = "Site names must all be unique"
		)
		assertthat::assert_that(
			assertthat::noNA(comm[[site_col]]),
			msg = "Site names must not include any missing data"
		)
		assertthat::assert_that(
			!isTRUE(grepl(site_col, "_obs$|_rand_|_obs_")),
			msg = "Name of 'site_col' cannot resemble names of column names in 'cpr_rand_test()' output"
		)
		comm <- tibble::column_to_rownames(comm, site_col)
	}
	#' @srrstats {G2.8} Convert matrix to dataframe
	#' @srrstats {UL1.2} Check that column names are unchanged after conversion
	if (inherits(comm, "matrix")) {
		comm_df <- data.frame(comm)
		assertthat::assert_that(
			isTRUE(
				all.equal(
					colnames(comm),
					colnames(comm_df)
				)
			),
			msg = "Column names of 'comm' changed after conversion from matrix to dataframe. Do any column names start with a number or include duplicates? This is not allowed."
		)
		comm <- comm_df
	}
	#' @srrstats {UL1.2} Check for default-looking rownames
	# Default rownames not allowed because phyloregion::dense2sparse() will convert them to NULL
	assertthat::assert_that(
		!identical(rownames(comm), as.character(seq(nrow(comm)))),
		msg = "'comm' cannot have default row names (consecutive integers from 1 to the number of rows)"
	)
	assertthat::assert_that(
		isTRUE(all(assertr::is_uniq(colnames(comm), allow.na = FALSE))),
		msg = "'comm' must have unique column names")
	assertthat::assert_that(
		isTRUE(all(assertr::is_uniq(rownames(comm), allow.na = FALSE))),
		msg = "'comm' must have unique rownames"
	)
	assertthat::assert_that(assertthat::noNA(colnames(comm)))
	assertthat::assert_that(assertthat::noNA(rownames(comm)))
	#' @srrstats {G2.15, UL1.1} don't assume non-missingness
	assertthat::assert_that(
		assertr::assert(
			comm, assertr::not_na, dplyr::everything(),
			success_fun = assertr::success_logical, error_fun = assertr::error_logical),
		msg = "No missing values allowed in 'comm'"
	)
	#' @srrstats {G2.11, UL1.1} check for class attributes in dataframe
	numeric_check <- NULL
	for (i in 1:ncol(comm)) {
		numeric_check[i] <- is.vector(comm[,i], mode = "numeric")
	}
	assertthat::assert_that(
		isTRUE(all(numeric_check)),
		msg = "All columns of 'comm' must be numeric"
	)
	#' @srrstats {G2.16, UL1.1} don't allow infinite values
	assertthat::assert_that(
		isTRUE(all(is.finite(unlist(comm)))),
		msg = "No infinite values allowed in 'comm'"
	)
	#' @srrstats {G2.4a} Convert all values in comm to integer
	comm <- dplyr::mutate(comm, dplyr::across(dplyr::everything(), as.integer))
	#' @srrstats {UL1.4} Check that all values in comm are >= 0
	assertthat::assert_that(
		isTRUE(all(unlist(comm) >= 0)), # values have been converted to integer, so OK to use equalities
		msg = "No negative values allowed in 'comm'"
	)
	#' @srrstats {UL1.4} Check for all 0s for a given site or species
	assertthat::assert_that(
		isTRUE(all(colSums(comm) > 0)),
		msg = "Every species in 'comm' must occur in at least one site"
	)
	assertthat::assert_that(
		isTRUE(all(rowSums(comm) > 0)),
		msg = "Every site in 'comm' must have at least once species"
	)
	#' @srrstats {UL1.4, UL2.0} Warn if community matrix is >95% present or absent and defaults are unchanged
	percent_present <- (sum(unlist(comm) > 0)) / (length(unlist(comm)))
	if (percent_present > 0.95 && n_reps_input == 100 && n_iterations_input == 10000) {
		warning("'comm' is > 95% presences (values > 1). Be sure that 'n_reps' and 'n_iterations' are sufficiently large to ensure adequate mixing of random communities")
	}
	if (percent_present < 0.05 && n_reps_input == 100 && n_iterations_input == 10000) {
		warning("'comm' is > 95% absences (zeros). Be sure that 'n_reps' and 'n_iterations' are sufficiently large to ensure adequate mixing of random communities")
	}

	# Check input: `phylo` ----
	assertthat::assert_that(
		is.list(phy) && inherits(phy, "phylo"),
		msg = "'phy' must be a list of class 'phylo'")
	# Check for phylo unique tip labels
	assertthat::assert_that(
		isTRUE(all(assertr::is_uniq(phy$tip.label, allow.na = FALSE))),
		msg = "All tip labels in 'phy' must be unique"
	)
	#' @srrstats {UL1.4} Check for non-negative branch lengths
	assertthat::assert_that(
		isTRUE(all(phy$edge.length %>=% 0)),
		msg = "'phy' may not have negative branchlengths"
	)
	assertthat::assert_that(is.numeric(phy$edge.length))
	assertthat::assert_that(assertthat::noNA(phy$edge.length))
	assertthat::assert_that(is.character(phy$tip.label))
	assertthat::assert_that(assertthat::noNA(phy$tip.label))
	assertthat::assert_that(
		!any(purrr::map_lgl(phy$edge.length, is.infinite)),
		msg = "'phy' may not have infinite branchlengths"
	)

	# Match input between `comm` and `phylo` ----
	# Match tips of tree and column names of community data frame:
	# Use only taxa that are in common between phylogeny and community
	subsetted_data <- picante::match.phylo.comm(phy = phy, comm = comm)
	phy <- subsetted_data[["phy"]]
	comm <- subsetted_data[["comm"]]

	# Make sure at least one taxon matches between comm and phy
	assertthat::assert_that(
		isTRUE(
			!is.null(phy) && !is.null(comm)
		),
		msg = "Tip names don't match between community and phylogeny"
	)

	assertthat::assert_that(
		isTRUE(
			all.equal(
				sort(colnames(comm)),
				sort(phy$tip.label)
			)
		),
		msg = "Tip names don't match between community and phylogeny"
	)

	# Verify that at least 5 species and sites are present
	assertthat::assert_that(ape::Ntip(phy) > 4, msg = "'phy' and 'comm' must share at least 5 species in common")
	assertthat::assert_that(nrow(comm) > 4, msg = "'comm' must include at least 5 sites")
	assertthat::assert_that(ncol(comm) > 4, msg = "'phy' and 'comm' must share at least 5 species in common")
	assertthat::assert_that(assertthat::not_empty(comm))

	# Prepare for calculations ----
	# Make alternative tree with equal branch lengths
	phy_alt <- phy
	# convert **non-zero** branch lengths to same value (1)
	non_zero_branches <- purrr::map_lgl(phy_alt$edge.length, ~!isTRUE(all.equal(., 0)))
	phy_alt$edge.length[non_zero_branches] <- rep(x = 1, times = length(phy_alt$edge.length[non_zero_branches]))
	# rescale so total phy length is 1
	phy_alt$edge.length <- phy_alt$edge.length / sum(phy_alt$edge.length)
	# rescale original phy so total length is 1
	phy$edge.length <- phy$edge.length / sum(phy$edge.length)

	# Make sparse community df
	comm_sparse <- phyloregion::dense2sparse(comm)

	# Calculate biodiversity metrics ----

	# Loop over random communities
	# - set up a progress bar
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
		if (!exists("pd_obs")) pd_obs <- phyloregion::PD(comm_sparse, phy)
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
		if (!exists("pe_obs")) pe_obs <- phyloregion::phylo_endemism(comm_sparse, phy, weighted = TRUE)
		pe_alt_obs <- phyloregion::phylo_endemism(comm_sparse, phy_alt, weighted = TRUE)
		ses_pe_alt <- get_ses(random_vals, pe_alt_obs, "pe_alt")
		rpe_obs <- pe_obs / pe_alt_obs
		ses_rpe <- get_ses(random_vals, rpe_obs, "rpe")
	}

	# Combine results
	# (don't use pipe, to maintain backwards compatibility with R < 4.1)
	results_df <- dplyr::bind_cols(
		ses_pd,
		ses_pd_alt,
		ses_rpd,
		ses_pe,
		ses_pe_alt,
		ses_rpe
	)

	results_df <- dplyr::mutate(results_df, site = rownames(comm))

	results_df <- tibble::column_to_rownames(results_df, "site")

	# If tibble was input, return tibble starting with the "site" column
	if (isTRUE(tbl_out)) {
		results_tbl <- tibble::rownames_to_column(results_df, site_col)
		results_tbl <- tibble::as_tibble(results_tbl)
		results_tbl <- dplyr::select(results_tbl, dplyr::all_of(site_col), dplyr::everything())
		return(results_tbl)
	} else {
		return(results_df)
	}

}
