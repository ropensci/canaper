#' Extract standard effect size (and other related statistics) for a single
#' diversity metric given random values and observed values of the metric
#'
#' @srrstats {G2.0a, G2.1a, G2.3b} Documents expectations on lengths, types of vector
#'   inputs, case-sensitivity
#' @param random_vals List of list of vectors. Each list of vectors is a
#'   biodiversity metric measured on a random community, in the same order as
#'   the rows in the input community.
#' @param obs_vals Numeric vector; observed values of the biodiversity metric
#' @param metric Character vector of length 1; Name of the metric (`pd`,
#'   `pd_alt`, `rpd`, `pe`, `pe_alt`, `rpe`) (case-sensitive).
#'
#' @return Dataframe
#' @examples
#' \dontrun{
#' data(phylocom)
#' phy <- phylocom$phy
#' comm <- phylocom$comm
#' subsetted_data <- match_phylo_comm(phy = phy, comm = comm)
#' phy <- subsetted_data[["phy"]]
#' comm <- subsetted_data[["comm"]]
#' phy_alt <- phy
#' phy_alt$edge.length <- rep(x = 1, times = length(phy_alt$edge.length))
#' phy_alt$edge.length <- phy_alt$edge.length / sum(phy_alt$edge.length)
#' phy$edge.length <- phy$edge.length / sum(phy$edge.length)
#' random_vals <-
#'   purrr::map(
#'     1:100,
#'     ~ calc_biodiv_random(comm, phy, phy_alt, "independentswap", 1000L, metrics = "pe")
#'   )
#' comm_sparse <- phyloregion::dense2sparse(comm)
#' pe_obs <- phyloregion::phylo_endemism(comm_sparse, phy, weighted = TRUE)
#' get_ses(random_vals, pe_obs, "pe")
#' }
#' @autoglobal
#'
#' @srrstats {G1.4, G1.4a} uses roxygen
#'
#' @noRd
get_ses <- function(random_vals, obs_vals, metric = c("pd", "pd_alt", "rpd", "pe", "pe_alt", "rpe")) {

	# Check input
	#' @srrstats {G2.1, G2.6} Check input types
	assertthat::assert_that(is.list(random_vals))
	assertthat::assert_that(is.numeric(obs_vals))
	assertthat::assert_that(assertthat::not_empty(obs_vals))
	assertthat::assert_that(assertthat::is.string(metric))
	assertthat::assert_that(assertthat::not_empty(metric))
	#' @srrstats {G2.3, G2.3a} # univariate char input
	assertthat::assert_that(
		all(metric %in% c("pd", "pd_alt", "rpd", "pe", "pe_alt", "rpe")),
		msg = "Biodiversity metrics may only be selected from 'pd', 'pd_alt', 'rpd', 'pe', 'pe_alt', or 'rpe'"
	)
	assertthat::assert_that(
		!isTRUE(is.null(names(obs_vals))),
		msg = "'obs_vals' must be named"
		)

	# Make sure the names match between `obs_vals` and `random_values`
	assertthat::assert_that(
	all(
		purrr::map_lgl(
			random_vals,
			~isTRUE(all.equal(names(.[[1]]), names(obs_vals)))
		)
	),
	msg = "Names don't match between 'obs_vals' and 'random_vals'"
	)

	# Transpose the list so we can extract the selected metric
	random_vals_trans <- purrr::transpose(random_vals)

	# Make sure the observed metric is among the random values
	assertthat::assert_that(
		metric %in% names(random_vals_trans),
		msg = "Observed metric missing from random values"
	)

	# Make a tibble with the random values as list-column,
	# and observed values for the selected metric
	results <-
		tibble::tibble(
			random_values = purrr::map(
				purrr::transpose(random_vals_trans[[metric]]),
				purrr::as_vector
			),
			obs_val = obs_vals
		)

	results <- dplyr::transmute(
		results,
		obs = obs_val,
		# Calculate SES
		rand_mean = purrr::map_dbl(random_values, ~ mean(., na.rm = TRUE)),
		rand_sd = purrr::map_dbl(random_values, ~ sd(., na.rm = TRUE)),
		obs_z = (obs_val - rand_mean) / rand_sd,
		# Count number of times observed value is higher than random values
		obs_c_upper = purrr::map2_dbl(.x = obs_val, .y = random_values, ~ count_higher(.x, .y)),
		# Count number of times observed value is lower than random values
		obs_c_lower = purrr::map2_dbl(.x = obs_val, .y = random_values, ~ count_lower(.x, .y)),
		# Count the number of non-NA random values used for comparison
		obs_q = purrr::map_dbl(random_values, ~sum(!is.na(.))),
		# Calculate p-value for upper tail
		obs_p_upper = obs_c_upper / obs_q,
		# Calculate p-value for lower tail
		obs_p_lower = obs_c_lower / obs_q
	)

	colnames(results) <- paste(metric, colnames(results), sep = "_")

	results
}
