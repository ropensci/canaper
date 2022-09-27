#' Make a palette of colors for plotting CANAPE results
#'
#' Palettes with the name 'mishler2014' correspond to colors used in Mishler et
#' al. 2014. This color scheme has been widely used since to show CANAPE
#' results, but some colors may not be distinguishable to people with
#' color vision deficiency (CVD).
#' 
#' Palettes with the name 'canaper1' or 'canaper2' are based on the Okabe-Ito
#' palette and should be more CVD-friendly.
#'
#' @param name Character vector of length 1; name of palette to select. Must be
#' one of 'misher2014', 'canaper1', or 'canaper2'
#' @param type Character vector of length 1; type of palette to select. Must be
#' one of 'endem' (endemism) or 'signif' (*p*-rank significance)
#' @references Mishler, B., Knerr, N., González-Orozco, C. et al.  (2014)
#'   Phylogenetic measures of biodiversity and neo- and paleo-endemism in
#'   Australian Acacia. Nat Commun, 5: 4473. \doi{10.1038/ncomms5473}
#' @return Named character vector; color hex codes
#' @export
cpr_make_pal <- function(
  name = c(
    "mishler2014", "canaper1", "canaper2"),
  type = c(
    "endem", "signif"
  )
  ) {
  # Check input: name
  assertthat::assert_that(assertthat::is.string(name))
  assertthat::assert_that(assertthat::noNA(name))
  assertthat::assert_that(
    name %in% c("mishler2014", "canaper1", "canaper2"),
    msg = "'name' may only include 'mishler2014', 'canaper1', or 'canaper2'"
  )
  # Check input: type
  assertthat::assert_that(assertthat::is.string(type))
  assertthat::assert_that(assertthat::noNA(type))
  assertthat::assert_that(
    type %in% c("endem", "signif"),
    msg = "'type' may only include 'endem' or 'signif'"
  )
  if (type == "endem" && type == "mishler2014") return(mishler2014_endem)
  if (type == "endem" && type == "canaper1") return(canaper1_endem)
  if (type == "endem" && type == "canaper2") return(canaper2_endem)
  if (type == "signif" && type == "mishler2014") return(mishler2014_signif)
  if (type == "signif" && type == "canaper1") return(canaper1_endem)
  if (type == "signif" && type == "canaper2") return(canaper2_endem)
  }