#' Make a palette of colors for plotting CANAPE results
#'
#' Palettes can also be loaded by name directly (see "Other colors" below).
#'
#' Palettes with the name 'mishler2014' correspond to colors used in Mishler et
#' al. 2014. This color scheme has been widely used to show CANAPE
#' results, but some colors may not be distinguishable to people with color
#' vision deficiency (CVD).
#'
#' Palettes with the name 'canaper1' or 'canaper2' are based on the Okabe-Ito
#' palette (endemism; Okabe & Ito 2002) or RColorBrewer (significance) and
#' should be CVD-friendly.
#'
#' Names of colors correspond to either endemism type or
#' *p*-rank output by [cpr_classify_endem()] or [cpr_classify_signif()],
#' respectively. Not all names are all available for both types.
#'
#' @param name Character vector of length 1; name of palette to select. Must be
#'   one of 'mishler2014', 'canaper1', 'canaper2', 'canaper3', or 'canaper4'
#' @param type Character vector of length 1; type of palette to select. Must be
#'  one of 'endem' (endemism) or 'signif' (*p*-rank significance)
#' @references Mishler, B., Knerr, N., González-Orozco, C. et al.  (2014)
#'   Phylogenetic measures of biodiversity and
#'   neo- and paleo-endemism in Australian Acacia. Nat Commun, 5: 4473.
#'   \doi{10.1038/ncomms5473}
#' @references Okabe, M., & Ito, K. (2002) Color
#'   Universal Design (CUD) - How to make figures and presentations that are
#'   friendly to Colorblind people. J*FLY Data
#'   Depository for Drosophila Researchers. <https://jfly.uni-koeln.de/color/>
#' @return Named character vector; color hex codes
#' @family colors
#' @autoglobal
#' @export
#' @examples
#' cpr_make_pal("mishler2014", "endem")
#' cpr_make_pal("canaper1", "endem")
#' scales::show_col(cpr_make_pal("mishler2014", "endem"))
cpr_make_pal <- function(name, type) {
  # Check input: name
  assertthat::assert_that(assertthat::is.string(name))
  assertthat::assert_that(assertthat::noNA(name))
  assertthat::assert_that(
    name %in% c("mishler2014", "canaper1", "canaper2", "canaper3", "canaper4"),
    msg = "'name' may only include 'mishler2014', 'canaper1', 'canaper2', 'canaper3', or 'canaper4'" # nolint
  )
  # Check input: type
  assertthat::assert_that(assertthat::is.string(type))
  assertthat::assert_that(assertthat::noNA(type))
  assertthat::assert_that(
    type %in% c("endem", "signif"),
    msg = "'type' may only include 'endem' or 'signif'"
  )
  color_spec <- paste(type, name, sep = "_")
  switch(color_spec,
    endem_mishler2014 = mishler_endem_cols,
    endem_canaper1 = cpr_endem_cols,
    endem_canaper2 = cpr_endem_cols_2,
    endem_canaper3 = cpr_endem_cols_3,
    endem_canaper4 = cpr_endem_cols_4,
    signif_mishler2014 = mishler_signif_cols,
    signif_canaper1 = cpr_signif_cols,
    signif_canaper2 = cpr_signif_cols_2,
    stop("No palette available with that name and type")
  )
}
