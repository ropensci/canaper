#' Classify Branches Using RWiBaLD
#'
#' Calculate range-weighted branch length difference (RWiBaLD) scores for all
#' branches in a phylogeny, then classify highly endemic branches as
#' neo-endemic, meso-endemic, or paleo-endemic.
#'
#' This implementation follows the core protocol from Mishler et al. (2026):
#' (1) compute branch-level RWiBaLD scores from an observed tree and an
#' equal-branch-length comparison tree, (2) identify highly endemic branches
#' using an elbow threshold on range-weighted branch length from the comparison
#' tree, and (3) classify key branches using separate elbow thresholds for
#' negative and positive RWiBaLD scores.
#'
#' @param comm Dataframe, tibble, or matrix; community data with sites as rows
#'   and species as columns.
#' @param phy List of class `phylo`; input phylogeny.
#' @param site_col Character vector of length 1; column in `comm` that
#'   identifies site names when `comm` is a tibble.
#' @param quiet Logical vector of length 1; should warnings be suppressed?
#'
#' @return Dataframe with one row per branch in `phy` and columns:
#' - `branch_id`: Branch index (row in `phy$edge`)
#' - `parent_node`: Parent node id
#' - `child_node`: Child node id
#' - `is_terminal`: `TRUE` if child is a tip
#' - `range_size`: Number of sites where the branch is present
#' - `branch_length_observed_tree`: Range-weighted length on observed tree
#' - `branch_length_comparison_tree`: Range-weighted length on comparison tree
#' - `rank_comparison_tree`: Rank (descending) of comparison-tree lengths
#' - `sorted_asc_bl_comparison_tree`: Rank (ascending) of comparison-tree
#'   lengths
#' - `rwibald_score`: Observed minus comparison range-weighted length
#' - `rwibald_score_rank`: Rank (ascending) of `rwibald_score`
#' - `key_branch`: `TRUE` for highly endemic branches
#' - `rwibald_type`: `neo-endemic`, `meso-endemic`, `paleo-endemic` for key
#'   branches; `NA` for non-key branches
#'
#' @references Mishler BD, Knerr N, Miller JT, Thornhill AH, Laffan SW (2026)
#'   Range-weighted branch length difference (RWiBaLD), a new method for
#'   distinguishing meso-endemism from neo-endemism and paleo-endemism.
#'   PLoS One 21(5): e0349623. \doi{10.1371/journal.pone.0349623}
#'
#' @examples
#' data(biod_example)
#' out <- cpr_rwibald(biod_example$comm, biod_example$phy)
#' head(out)
#'
#' @export
cpr_rwibald <- function(comm, phy, site_col = "site", quiet = FALSE) {
  assertthat::assert_that(assertthat::is.flag(quiet))
  assertthat::assert_that(
    inherits(comm, "data.frame") | inherits(comm, "matrix"),
    msg = "'comm' must be of class 'data.frame' or 'matrix'"
  )

  if (tibble::is_tibble(comm)) {
    assertthat::assert_that(assertthat::is.string(site_col))
    assertthat::assert_that(assertthat::noNA(site_col))
    assertthat::assert_that(
      site_col %in% colnames(comm),
      msg = "'site_col' must be one of the column names of 'comm'"
    )
    comm <- tibble::column_to_rownames(comm, site_col)
  }

  if (inherits(comm, "matrix")) {
    comm <- data.frame(comm)
  }

  assertthat::assert_that(
    is.list(phy) && inherits(phy, "phylo"),
    msg = "'phy' must be a list of class 'phylo'"
  )
  assertthat::assert_that(
    isTRUE(all(assertr::is_uniq(phy$tip.label, allow.na = FALSE))),
    msg = "All tip labels in 'phy' must be unique"
  )
  assertthat::assert_that(
    is.numeric(phy$edge.length),
    msg = "'phy$edge.length' must be numeric"
  )
  assertthat::assert_that(
    assertthat::noNA(phy$edge.length),
    msg = "'phy$edge.length' may not include missing values"
  )
  assertthat::assert_that(
    isTRUE(all(phy$edge.length %>=% 0)),
    msg = "'phy' may not have negative branchlengths"
  )

  assertthat::assert_that(
    isTRUE(all(assertr::is_uniq(colnames(comm), allow.na = FALSE))),
    msg = "'comm' must have unique column names"
  )
  assertthat::assert_that(
    isTRUE(all(assertr::is_uniq(rownames(comm), allow.na = FALSE))),
    msg = "'comm' must have unique rownames"
  )
  assertthat::assert_that(assertthat::noNA(colnames(comm)))
  assertthat::assert_that(assertthat::noNA(rownames(comm)))
  assertthat::assert_that(
    assertr::assert(
      comm,
      assertr::not_na,
      dplyr::everything(),
      success_fun = assertr::success_logical,
      error_fun = assertr::error_logical
    ),
    msg = "No missing values allowed in 'comm'"
  )

  numeric_check <- NULL
  for (i in seq_along(comm)) {
    numeric_check[i] <- is.vector(comm[, i], mode = "numeric")
  }
  assertthat::assert_that(
    isTRUE(all(numeric_check)),
    msg = "All columns of 'comm' must be numeric"
  )

  comm <- dplyr::mutate(comm, dplyr::across(dplyr::everything(), as.integer))
  assertthat::assert_that(
    isTRUE(all(unlist(comm) >= 0)),
    msg = "No negative values allowed in 'comm'"
  )
  assertthat::assert_that(
    isTRUE(all(colSums(comm) > 0)),
    msg = "Every species in 'comm' must occur in at least one site"
  )
  assertthat::assert_that(
    isTRUE(all(rowSums(comm) > 0)),
    msg = "Every site in 'comm' must have at least one species"
  )

  subsetted <- match_phylo_comm(phy = phy, comm = comm, quiet = quiet)
  phy <- subsetted[["phy"]]
  comm <- subsetted[["comm"]]

  assertthat::assert_that(
    ape::Ntip(phy) >= 2,
    msg = "'phy' and 'comm' must share at least 2 species in common"
  )
  assertthat::assert_that(
    nrow(comm) >= 1,
    msg = "'comm' must include at least 1 site"
  )

  phy <- cpr_normalize_tree(phy)
  phy_alt <- cpr_make_equal_tree(phy)

  comm_pa <- as.matrix(comm)
  comm_pa[comm_pa > 0] <- 1
  comm_pa <- comm_pa[, phy$tip.label, drop = FALSE]

  edge_tip <- cpr_edge_tip_matrix(phy)
  site_branch_counts <- comm_pa %*% t(edge_tip)
  site_branch_presence <- site_branch_counts > 0
  range_size <- colSums(site_branch_presence)

  assertthat::assert_that(
    isTRUE(all(range_size > 0)),
    msg = "All branches must occur in at least one site"
  )

  bl_observed <- phy$edge.length / range_size
  bl_comparison <- phy_alt$edge.length / range_size
  rwibald_score <- bl_observed - bl_comparison

  rwibald_score_rank <- rank(rwibald_score, ties.method = "first")
  key_threshold <- cpr_get_elbow(bl_comparison)$y
  key_branch <- bl_comparison %>=% key_threshold

  left_df <- data.frame(
    score = rwibald_score[rwibald_score <= 0],
    rank = rwibald_score_rank[rwibald_score <= 0]
  )
  right_df <- data.frame(
    score = rwibald_score[rwibald_score >= 0],
    rank = rwibald_score_rank[rwibald_score >= 0]
  )

  neo_cutoff <- NA_integer_
  paleo_cutoff <- NA_integer_

  if (nrow(left_df) > 0) {
    left_threshold <- cpr_get_elbow(left_df$score)$y
    neo_cutoff <- max(left_df$rank[left_df$score %<=% left_threshold])
  }
  if (nrow(right_df) > 0) {
    right_threshold <- cpr_get_elbow(right_df$score)$y
    paleo_cutoff <- min(right_df$rank[right_df$score %>=% right_threshold])
  }

  rwibald_type <- rep(NA_character_, times = length(rwibald_score))
  key_idx <- which(key_branch)

  if (length(key_idx) > 0) {
    rwibald_type[key_idx] <- "meso-endemic"
    if (!is.na(neo_cutoff)) {
      rwibald_type[key_idx[rwibald_score_rank[key_idx] <= neo_cutoff]] <-
        "neo-endemic"
    }
    if (!is.na(paleo_cutoff)) {
      rwibald_type[key_idx[rwibald_score_rank[key_idx] >= paleo_cutoff]] <-
        "paleo-endemic"
    }
  }

  data.frame(
    branch_id = seq_len(nrow(phy$edge)),
    parent_node = phy$edge[, 1],
    child_node = phy$edge[, 2],
    is_terminal = phy$edge[, 2] <= ape::Ntip(phy),
    range_size = range_size,
    branch_length_observed_tree = bl_observed,
    branch_length_comparison_tree = bl_comparison,
    rank_comparison_tree = rank(
      -bl_comparison,
      ties.method = "min"
    ),
    sorted_asc_bl_comparison_tree = rank(
      bl_comparison,
      ties.method = "first"
    ),
    rwibald_score = rwibald_score,
    rwibald_score_rank = rwibald_score_rank,
    key_branch = key_branch,
    rwibald_type = rwibald_type,
    stringsAsFactors = FALSE
  )
}

#' @noRd
cpr_normalize_tree <- function(phy) {
  phy_out <- phy
  phy_out$edge.length <- phy_out$edge.length / sum(phy_out$edge.length)
  phy_out
}

#' @noRd
cpr_make_equal_tree <- function(phy) {
  phy_alt <- phy
  non_zero <- purrr::map_lgl(phy_alt$edge.length, ~ !isTRUE(all.equal(., 0)))
  phy_alt$edge.length[non_zero] <- 1
  cpr_normalize_tree(phy_alt)
}

#' @noRd
cpr_edge_tip_matrix <- function(phy) {
  edge <- phy$edge
  n_tip <- ape::Ntip(phy)
  n_edge <- nrow(edge)
  max_node <- max(edge)

  children <- split(edge[, 2], edge[, 1])
  memo <- vector("list", max_node)

  get_desc <- function(node) {
    if (!is.null(memo[[node]])) {
      return(memo[[node]])
    }
    if (node <= n_tip) {
      memo[[node]] <<- node
      return(node)
    }
    node_children <- children[[as.character(node)]]
    if (is.null(node_children)) {
      stop("Could not identify descendants for node ", node)
    }
    desc <- unlist(lapply(node_children, get_desc), use.names = FALSE)
    memo[[node]] <<- desc
    desc
  }

  out <- matrix(FALSE, nrow = n_edge, ncol = n_tip)
  colnames(out) <- phy$tip.label

  for (i in seq_len(n_edge)) {
    desc <- get_desc(edge[i, 2])
    out[i, desc] <- TRUE
  }

  out
}

#' @noRd
cpr_get_elbow <- function(data) {
  assertthat::assert_that(is.numeric(data))
  assertthat::assert_that(assertthat::noNA(data))
  assertthat::assert_that(length(data) > 0)

  n_pts <- length(data)
  x_coords <- seq_len(n_pts)
  y_coords <- data[order(data)]

  x1 <- x_coords[1]
  y1 <- y_coords[1]

  x_vec <- x_coords - x1
  y_vec <- y_coords - y1
  x_vec_max <- x_vec[n_pts]
  y_vec_max <- y_vec[n_pts]

  normalizer <- sqrt(x_vec_max^2 + y_vec_max^2)
  if (isTRUE(all.equal(normalizer, 0))) {
    return(list(x = x_coords[1], y = y_coords[1]))
  }

  x_vec_max <- x_vec_max / normalizer
  y_vec_max <- y_vec_max / normalizer

  v_x <- x_coords - x1
  v_y <- y_coords - y1
  scalar_prod <- v_x * x_vec_max + v_y * y_vec_max

  vec_to_line_x <- v_x - scalar_prod * x_vec_max
  vec_to_line_y <- v_y - scalar_prod * y_vec_max

  dist_to_line <- sqrt(vec_to_line_x^2 + vec_to_line_y^2)
  i_max <- which.max(dist_to_line)

  list(x = x_coords[i_max], y = y_coords[i_max])
}
