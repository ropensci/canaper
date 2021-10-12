#' Community matrix and phylogeny of Acacia in Australia
#'
#' Dataset of Australian \emph{Acacia} from Mishler et al. 2014 (Nat. Comm.)
#'
#' @format A list with two elements:
#' \describe{
#'   \item{phy}{Phylogeny of Australian \emph{Acacia} (list of class "phylo"). Tip
#'   labels are specific epithet, except for the outgroup, which includes genus
#'   and specific epithet. Includes 508 ingroup taxa (genus \emph{Acacia}) and two
#'   outgroup taxa.}
#'   \item{comm}{Community matrix of Australian \emph{Acacia} (dataframe). Column names
#'   are specific epithet of each species. Row names are centroids of 50km grid
#'   cells in Australian Albers equal area EPSG:3577 projection. 3037 rows
#'   (sites) x 506 columns (species). Data are counts, i.e., the number of times
#'   a species was observed in a grid cell.}
#' }
#' @source Mishler, B., Knerr, N., González-Orozco, C. et al. Phylogenetic
#' measures of biodiversity and neo- and paleo-endemism in Australian Acacia.
#' Nat Commun 5, 4473 (2014). \doi{10.1038/ncomms5473}
#'
#' @srrstats {G1.4} uses roxygen
#' @srrstats {G5.1} dataset is available (and documented)
#' @examples
#' # Example phylogeny
#' library(ape)
#' acacia$phy
#' # Example community
#' acacia$comm[1:5,1:5]
"acacia"

#' Color palette for plotting results of randomization test
#'
#' Character vector with names corresponding to significance levels
#' and values corresponding to color codes.
#'
#' @srrstats {G1.4} uses roxygen
#'
#' @examples
#' cpr_signif_cols
#' \dontrun{
#' scales::show_col(cpr_signif_cols)
#' }
"cpr_signif_cols"

#' Color palette for plotting results of CANAPE
#'
#' Character vector with names corresponding to endemism types
#' and values corresponding to color codes.
#'
#' @srrstats {G1.4} uses roxygen
#'
#' @examples
#' cpr_endem_cols
#' \dontrun{
#' scales::show_col(cpr_endem_cols)
#' }
"cpr_endem_cols"
