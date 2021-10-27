#' Count number of times one number is higher than others
#'
#' @param x Number to count
#' @param y Vector of numbers to compare
#' @param na.rm = Logical; should NA values in the comparison
#' vector be removed before making comparison?
#'
#' @return Number of times x is higher than y
#'
#' @srrstats {G1.4, G1.4a} uses roxygen
#'
#' @examples
#' \dontrun{
#' count_higher(4, 1:10)
#' count_higher(4, c(1:10, NaN))
#' }
#' @noRd
count_higher <- function(x, y, na.rm = TRUE) {

  #' @srrstats {G2.1, G2.6} Check input types and lengths
  assertthat::assert_that(assertthat::is.number(x))
  assertthat::assert_that(is.numeric(y))

  # remove any NAs before making comparison
  if (isTRUE(na.rm)) y <- y[!is.na(y)]

  # if comparison is zero length, return NA
  if (length(y) == 0) {
    return(NaN)
  }

  sum((x > y))
}

#' Count number of times one number is lower than others
#'
#' @param x Number to count
#' @param y Vector of numbers to compare
#' @param na.rm = Logical; should NA values in the comparison
#' vector be removed before making comparison?
#'
#' @return Number of times x is lower than y
#'
#' @srrstats {G1.4, G1.4a} uses roxygen
#'
#' @examples
#' \dontrun{
#' count_lower(4, 1:10)
#' count_lower(NaN, 1:10)
#' }
#' @noRd
count_lower <- function(x, y, na.rm = TRUE) {

  #' @srrstats {G2.1, G2.6} Check input types and lengths
  assertthat::assert_that(assertthat::is.number(x))
  assertthat::assert_that(is.numeric(y))

  # remove any NAs before making comparison
  if (isTRUE(na.rm)) y <- y[!is.na(y)]

  # if comparison is zero length, return NA
  if (length(y) == 0) {
    return(NaN)
  }

  sum((x < y))
}

#' Version of `<` that allows for modifying tolerance of equality test (`all.equal()`)
#'
#' Non-vectorized version. Is `x` lesser than `y`?
#'
#' @srrstats {G3.0} Uses appropriate tolerances for approximate equality
#' @param x Numeric vector of length 1
#' @param y Numeric vector of length 1
#' @return Logical vector of length 1
#' @noRd
lesser_than_single <- function(x, y) {
  #' @srrstats {G2.1, G2.6} Check input types and lengths
  assertthat::assert_that(assertthat::is.number(x))
  assertthat::assert_that(assertthat::is.number(y))
  # only true if x and y are NOT equal, and x is less than y
  !isTRUE(all.equal(x, y)) && (x < y)
}

#' Version of `<` that allows for modifying tolerance of equality test (`all.equal()`)
#'
#' Vectorized version. Is `x` lesser than `y`?
#'
#' `%>%` is already taken by the pipe, so don't use `%>%` or `%<%` as name
#'
#' @srrstats {G3.0} Uses appropriate tolerances for approximate equality
#' @param x Numeric vector
#' @param y Numeric vector
#' @return Logical vector
#' @noRd
`%lesser%` <- function(x, y) {
  #' @srrstats {G2.1, G2.6} Check input types
  assertthat::assert_that(is.numeric(x))
  assertthat::assert_that(is.numeric(y))
  purrr::map2_lgl(x, y, lesser_than_single)
}

#' Version of `=<` that allows for modifying tolerance of equality test (`all.equal()`)
#'
#' Non-vectorized version. Is `x` lesser than or equal to `y`?
#'
#' @srrstats {G3.0} Uses appropriate tolerances for approximate equality
#' @param x Numeric vector of length 1
#' @param y Numeric vector of length 1
#' @return Logical vector of length 1
#' @noRd
lesser_than_or_equal_single <- function(x, y) {
  #' @srrstats {G2.1, G2.6} Check input types and lengths
  assertthat::assert_that(assertthat::is.number(x))
  assertthat::assert_that(assertthat::is.number(y))
  # true if x and y are equal OR x is less than y
  isTRUE(all.equal(x, y)) || (x < y)
}

#' Version of `=<` that allows for modifying tolerance of equality test (`all.equal()`)
#'
#' Vectorized version. Is `x` lesser than or equal to `y`?
#'
#' @srrstats {G3.0} Uses appropriate tolerances for approximate equality
#' @param x Numeric vector
#' @param y Numeric vector
#' @return Logical vector
#' @noRd
`%<=%` <- function(x, y) {
  #' @srrstats {G2.1, G2.6} Check input types
  assertthat::assert_that(is.numeric(x))
  assertthat::assert_that(is.numeric(y))
  purrr::map2_lgl(x, y, lesser_than_or_equal_single)
}

#' Version of `>` that allows for modifying tolerance of equality test (`all.equal()`)
#'
#' Non-vectorized version. Is `x` greater than `y`?
#'
#' @srrstats {G3.0} Uses appropriate tolerances for approximate equality
#' @param x Numeric vector of length 1
#' @param y Numeric vector of length 1
#' @return Logical vector of length 1
#' @noRd
greater_than_single <- function(x, y) {
  #' @srrstats {G2.1, G2.6} Check input types and lengths
  assertthat::assert_that(assertthat::is.number(x))
  assertthat::assert_that(assertthat::is.number(y))
  # only true if x and y are NOT equal, and x is greater than y
  !isTRUE(all.equal(x, y)) && (x > y)
}

#' Version of `>` that allows for modifying tolerance of equality test (`all.equal()`)
#'
#' Vectorized version. Is `x` greater than `y`?
#'
#' `%>%` is already taken by the pipe, so don't use `%>%` or `%<%` as name
#'
#' @srrstats {G3.0} Uses appropriate tolerances for approximate equality
#' @param x Numeric vector
#' @param y Numeric vector
#' @return Logical vector
#' @noRd
`%greater%` <- function(x, y) {
  #' @srrstats {G2.1, G2.6} Check input types
  assertthat::assert_that(is.numeric(x))
  assertthat::assert_that(is.numeric(y))
  purrr::map2_lgl(x, y, greater_than_single)
}

#' Version of `>=` that allows for modifying tolerance of equality test (`all.equal()`)
#'
#' Non-vectorized version. Is `x` greater than or equal to `y`?
#'
#' @srrstats {G3.0} Uses appropriate tolerances for approximate equality
#' @param x Numeric vector of length 1
#' @param y Numeric vector of length 1
#' @return Logical vector of length 1
#' @noRd
greater_than_or_equal_single <- function(x, y) {
  #' @srrstats {G2.1, G2.6} Check input types and lengths
  assertthat::assert_that(assertthat::is.number(x))
  assertthat::assert_that(assertthat::is.number(y))
  # true if x and y are equal OR x is greater than y
  isTRUE(all.equal(x, y)) || (x > y)
}

#' Version of `>=` that allows for modifying tolerance of equality test (`all.equal()`)
#'
#' Vectorized version. Is `x` greater than `y`?
#'
#' @srrstats {G3.0} Uses appropriate tolerances for approximate equality
#' @param x Numeric vector
#' @param y Numeric vector
#' @return Logical vector
#' @noRd
`%>=%` <- function(x, y) {
  #' @srrstats {G2.1, G2.6} Check input types
  assertthat::assert_that(is.numeric(x))
  assertthat::assert_that(is.numeric(y))
  purrr::map2_lgl(x, y, greater_than_or_equal_single)
}

# Print phylogenies nicely
#' @importFrom ape print.phylo
NULL

#' Match taxa between a community data matrix and a phylogeny
#'
#' @param phy List of class "phylo"; input phylogeny
#' @param comm Dataframe or matrix; community data, with species (taxa) in columns
#' and sites (communities) in rows.
#'
#' @return List with two items
#' - `comm`: Community data matrix trimmed to only species in common between `phy` and comm`
#' - `phy`: Phylogeny data matrix trimmed to only species in common between `phy` and comm`
#' @noRd
#'
match_phylo_comm <- function(phy, comm) {
  if (!(is.data.frame(comm) | is.matrix(comm))) {
    stop("Community data should be a data.frame or matrix with samples in rows and taxa in columns")
  }
  res <- list()
  phytaxa <- phy$tip.label
  commtaxa <- colnames(comm)
  if (is.null(commtaxa)) {
    stop("Community data set lacks taxa (column) names, these are required to match phylogeny and community data")
  }
  if (!all(commtaxa %in% phytaxa)) {
    warning(paste(
      "Dropping taxa from the community because they are not present in the phylogeny: \n",
      paste(setdiff(commtaxa, phytaxa), collapse = ", ")
    ))
    comm <- comm[, intersect(commtaxa, phytaxa)]
    commtaxa <- colnames(comm)
  }
  if (any(!(phytaxa %in% commtaxa))) {
    warning(paste(
      "Dropping tips from the tree because they are not present in the community data: \n",
      paste(setdiff(phytaxa, commtaxa), collapse = ", ")
    ))
    res$phy <- ape::drop.tip(phy, setdiff(phytaxa, commtaxa))
  } else {
    res$phy <- phy
  }
  res$comm <- comm[, res$phy$tip.label]
  return(res)
}

# Functions copied from phyloregion v1.0.6 under AGPL-3 ----

# Corresponds to phyloregion::dense2sparse()
dense2sparse <- function(x) {
  x <- as.matrix(x)
  Matrix::Matrix(x, sparse = TRUE)
}

# Corresponds to phyloregion:::phylo_community()
phylo_community <- function(x, phy) {
  el <- numeric(max(phy$edge))
  el[phy$edge[, 2]] <- phy$edge.length
  x <- x[, phy$tip.label]
  anc <- phangorn::Ancestors(phy, seq_along(phy$tip.label))
  anc <- mapply(c, seq_along(phy$tip.label), anc, SIMPLIFY = FALSE)
  M <- Matrix::sparseMatrix(as.integer(rep(
    seq_along(anc),
    lengths(anc)
  )), as.integer(unlist(anc)), x = 1L)
  commphylo <- x %*% M
  commphylo@x[commphylo@x > 1e-08] <- 1
  list(Matrix = commphylo, edge.length = el)
}

# Corresponds to phyloregion::PD()
PD <- function(x, phy) {
  if (!methods::is(x, "sparseMatrix")) {
    stop("x needs to be a sparse matrix!")
  }
  if (length(setdiff(colnames(x), phy$tip.label)) > 0) {
    stop("There are species labels in community matrix missing in the tree!")
  }
  if (length(setdiff(phy$tip.label, colnames(x))) > 0) {
    phy <- ape::keep.tip(phy, intersect(phy$tip.label, colnames(x)))
  }
  x <- x[, intersect(phy$tip.label, colnames(x))]
  z <- phylo_community(x, phy)
  (z$Matrix %*% z$edge.length)[, 1]
}

# Corresponds to phyloregion::phylo_endemism()
phylo_endemism <- function(x, phy, weighted = TRUE) {
  if (length(setdiff(colnames(x), phy$tip.label)) > 0) {
    stop("There are species labels in community matrix missing in the tree!")
  }
  if (length(setdiff(phy$tip.label, colnames(x))) > 0) {
    phy <- ape::keep.tip(phy, intersect(phy$tip.label, colnames(x)))
  }
  comm_phylo <- phylo_community(x, phy)
  weights <- comm_phylo$Matrix %*% Matrix::Diagonal(x = 1 / Matrix::colSums(comm_phylo$Matrix))
  if (weighted == FALSE) {
    weights[weights < 1] <- 0
  }
  pd <- (weights %*% comm_phylo$edge.length)[, 1]
  pd <- pd[row.names(x)]
  return(pd)
}
