#' Calculate diversity metrics for a single random community
#'
#' For description of metrics available, see \code{\link{cpr_rand_test}()}
#'
#' @srrstats {G2.0a, G2.1a, G2.3b} Documents expectations on lengths, types of
#' vector inputs, case-sensitivity
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
#' set.seed(12345)
#' phy <- biod_example$phy
#' comm <- biod_example$comm
#' phy_alt <- phy
#' phy_alt$edge.length <- rep(x = 1, times = length(phy_alt$edge.length))
#' phy_alt$edge.length <- phy_alt$edge.length / sum(phy_alt$edge.length)
#' phy$edge.length <- phy$edge.length / sum(phy$edge.length)
#' calc_biodiv_random(
#'   comm, phy, phy_alt, "tswap", 1000,
#'   metrics = c("pd", "pe", "pd_alt")
#' )
#' @srrstats {G1.4, G1.4a} uses roxygen
#'
#' @noRd
calc_biodiv_random <- function(
  comm, phy, phy_alt, null_model, n_iterations = 1, thin = 1,
  metrics = c("pd", "rpd", "pe", "rpe", "pd_alt", "pe_alt"),
  seed = NULL
) {
  # Generate random community
  random_comm <- cpr_rand_comm_intern(
    comm,
    null_model = null_model,
    n_iterations = n_iterations, thin = thin, seed = seed
  )
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
  if ("pd_alt" %in% metrics) {
    pd_alt <-
      phyloregion::PD(random_comm_sparse, phy_alt)
  }
  # pd_alt is inferred by rpd
  if ("rpd" %in% metrics) {
    if (is.null(pd)) pd <- phyloregion::PD(random_comm_sparse, phy)
    if (is.null(pd_alt)) pd_alt <- phyloregion::PD(random_comm_sparse, phy_alt)
    rpd <- pd / pd_alt
  }
  # pe_alt is inferred by rpe
  if ("pe" %in% metrics) {
    pe <- phyloregion::phylo_endemism(
      random_comm_sparse, phy,
      weighted = TRUE
    )
  }
  if ("pe_alt" %in% metrics) {
    pe_alt <- phyloregion::phylo_endemism(
      random_comm_sparse, phy_alt,
      weighted = TRUE
    )
  }
  if ("rpe" %in% metrics) {
    if (is.null(pe)) {
      pe <- phyloregion::phylo_endemism(
        random_comm_sparse, phy,
        weighted = TRUE
      )
    }
    if (is.null(pe_alt)) {
      pe_alt <- phyloregion::phylo_endemism(
        random_comm_sparse, phy_alt,
        weighted = TRUE
      )
    }
    rpe <- pe / pe_alt
  }

  # Output non-NULL results
  # (note that pd_alt and pe_alt will be included if rpd or rpe were included
  # in `metrics`)
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
