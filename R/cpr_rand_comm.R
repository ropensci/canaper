#' Randomize a single community matrix
#'
#' Note that binary null models return a binary matrix, even if an abundance
#' matrix was used as input.
#'
#' @param comm Dataframe or matrix; input community data with
#'   sites (communities) as rows and species as columns. Values of each cell are
#'   the presence/absence (0 or 1) or number of individuals (abundance) of each
#'   species in each site.
#' @param null_model Character vector of length 1 or object of class `commsim`;
#'   either the name of the model to use for generating random communities (null
#'   model), or a custom null model. For full list of available predefined null
#'   models, see the help file of [vegan::commsim()], or run
#'   [vegan::make.commsim()]. An object of class `commsim` can be generated with
#'   [vegan::commsim()] (see Examples).
#' @param n_iterations Numeric vector of length 1; number of iterations for
#'   sequential null models. Ignored by non-sequential null models.
#' @param thin Numeric vector of length 1; thinning parameter used by some
#'   null models in `vegan` (e.g., `quasiswap`); ignored for other models.
#' @param seed Integer vector of length 1 or NULL; random seed that will be used
#'   in a call to `set.seed()` before randomizing the matrix. Default (`NULL`)
#'   will not change the random generator state.
#' @return Matrix
#' @export
#' @examples
#' # Check list of available pre-defined null models in vegan
#' vegan::make.commsim()
#'
#' # Binary null model produces binary output
#' data(phylocom)
#' cpr_rand_comm(phylocom$sample, "swap", 100)
#'
#' # Quantitative null model produces quantitative output
#' cpr_rand_comm(phylocom$sample, "swap_count", 100)
#'
#' # How to use a custom null model
#' # 1. Define a randomizing function, e.g. re-sample the matrix while
#' # preserving total number of presences (same as the "r00" model)
#' randomizer <- function(x, n, ...)
#' 	array(replicate(n, sample(x)), c(dim(x), n))
#'
#' # 2. Generate a commsim object
#' cs_object <- vegan::commsim(
#'   "r00_model", fun = randomizer, binary = TRUE,
#'   isSeq = FALSE, mode = "integer")
#'
#' # 3. Generate the null community
#' cpr_rand_comm(phylocom$sample, cs_object, 100)
#'
cpr_rand_comm <- function(comm, null_model, n_iterations = 1, thin = 1, seed = NULL) {

	#' @srrstats {G2.1, G2.6} Check input types and lengths
	# - comm
	assertthat::assert_that(inherits(comm, "data.frame") | inherits(comm, "matrix"),
													msg = "'comm' must be of class 'data.frame' or 'matrix'")
	assertthat::assert_that(
		isTRUE(all(unique(purrr::map_chr(comm, class)) %in% c("numeric", "integer"))),
		msg = "All columns of 'comm' must be numeric or integer class"
	)
	# - null_model
	assertthat::assert_that(
		assertthat::is.string(null_model) | inherits(null_model, "commsim"),
		msg = "'null_model' must be a string (character vector of length 1) or an object of class 'commsim'"
	)
	if (isTRUE(assertthat::is.string(null_model))) {
		assertthat::assert_that(assertthat::not_empty(comm))
		assertthat::assert_that(assertthat::noNA(null_model))
		assertthat::assert_that(
			isTRUE(null_model %in% vegan::make.commsim()),
			msg = paste0("'null_model' must be one of: '", paste0(vegan::make.commsim(), collapse = "', '"), "'")
		)
	}
	# - n_iterations
	assertthat::assert_that(assertthat::is.number(n_iterations))
	assertthat::assert_that(assertthat::noNA(n_iterations))
	assertthat::assert_that(is.finite(n_iterations))
	n_iterations <- as.integer(n_iterations)
	assertthat::assert_that(is.integer(n_iterations))
	assertthat::assert_that(n_iterations > 0, msg = "'n_iterations' must be > 0")
	# - thin
	assertthat::assert_that(assertthat::is.number(thin))
	assertthat::assert_that(assertthat::noNA(thin))
	assertthat::assert_that(is.finite(thin))
	thin <- as.integer(thin)
	assertthat::assert_that(is.integer(thin))
	assertthat::assert_that(thin > 0, msg = "'thin' must be > 0")
	# - seed
	assertthat::assert_that(is.numeric(seed) | is.null(seed))

	# Convert to matrix
	comm <- as.matrix(comm)

	# Initiate null model
	null_model <- vegan::nullmodel(comm, null_model)

	# Randomize matrix
	# just take the first simulated community after n_iterations - 1
	stats::simulate(null_model, nsim = 1, thin = thin, burnin = n_iterations - 1, seed = seed)[,,1]

}
