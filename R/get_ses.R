#' Extract standard effect size (and other related statistics) for a single
#' diversity metric given random values and observed values of the metric
#'
#' @param random_vals List of list of vectors. Each list of vectors is a biodiversity metric measured on a
#' random community, in the same order as the rows in the input community.
#' @param obs_vals Observed values of the biodiversity metric
#' @param metric Name of the metric ("mpd", "mntd", "mpd_morph", "mntd_morph", "pd", "pe", or "rpe")
#'
#' @return Tibble
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
#' random_vals <-
#'   purrr::map(
#'     1:100,
#'     ~ calc_biodiv_random(comm, phy, phy_alt, "independentswap", 1000, metrics = "pe")
#'   )
#' comm_sparse <- phyloregion::dense2sparse(comm)
#' pe_obs <- phyloregion::phylo_endemism(comm_sparse, phy, weighted = TRUE)
#' get_ses(random_vals, pe_obs, "pe")
#' }
#' @autoglobal
#' @keywords internal
get_ses <- function(random_vals, obs_vals, metric) {
	assertthat::assert_that(assertthat::is.string(metric))

	assertthat::assert_that(
		all(metric %in% c("pd", "pd_alt", "rpd", "pe", "pe_alt", "rpe")),
		msg = "Biodiversity metrics may only be selected from 'pd', 'rpd', 'pe', or 'rpe'"
	)

	random_vals_trans <- purrr::transpose(random_vals)

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
		obs_q = purrr::map_dbl(random_values, ~ length(magrittr::extract(., !is.na(.)))),
		# Calculate p-value for upper tail
		obs_p_upper = obs_c_upper / obs_q,
		# Calculate p-value for lower tail
		obs_p_lower = obs_c_lower / obs_q
	)

	colnames(results) <- paste(metric, colnames(results), sep = "_")

	results
}
