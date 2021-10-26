#' Classify phylogenetic endemism
#'
#' Given the results of [cpr_rand_test()], classifies phylogenetic endemism
#' according to CANAPE scheme of Mishler et al. 2014.
#'
#' For a summary of the classification scheme, see:
#' <http://biodiverse-analysis-software.blogspot.com/2014/11/canape-categorical-analysis-of-palaeo.html>
#'
#' @srrstats {G1.3} defines terminology:
#' @param df Input data frame. Must have the following columns:
#' - `pe_obs_p_upper`: Upper *p*-value comparing observed phylogenetic endemism to random values
#' - `pe_alt_obs_p_upper`: Upper *p*-value comparing observed phylogenetic endemism on alternate tree to random values
#' - `rpe_obs_p_upper`: Upper *p*-value comparing observed relative phylogenetic endemism to random values
#'
#' @return Dataframe with column `endem_type` (character) added. Values of
#'   `endem_type` type include `paleo` (paleoendemic), `neo` (neoendemic), `not
#'   significant` (what it says), `mixed` (mixed endemism), and `super`
#'   (super-endemic; both `pe_obs` and `pe_obs_alt` are highly significant).
#'
#' @srrstats {G1.0} Cites original ref:
#' @references Mishler, B., Knerr, N., González-Orozco, C. et al.  (2014)
#'   Phylogenetic measures of biodiversity and neo- and paleo-endemism in
#'   Australian Acacia. Nat Commun, 5: 4473. \doi{10.1038/ncomms5473}
#'
#' @examples
#' set.seed(12345)
#' data(phylocom)
#' rand_test <- cpr_rand_test(
#'   phylocom$comm, phylocom$phy,
#'   null_model = "curveball", metrics = c("pe", "rpe")
#' )
#' cpr_classify_endem(rand_test)
#' @srrstats {G1.4} uses roxygen
#' @export
cpr_classify_endem <- function(df) {

  # Check input
  #' @srrstats {G2.1, G2.6} Check input types
  assertthat::assert_that(
    inherits(df, "data.frame"),
    msg = "'df' must be of class 'data.frame'"
  )
  assertthat::assert_that(
    isTRUE(all(c("pe_obs_p_upper", "pe_alt_obs_p_upper", "rpe_obs_p_upper") %in% colnames(df))),
    msg = "'df' must include the following columns: 'pe_obs_p_upper', 'pe_alt_obs_p_upper', 'rpe_obs_p_upper'"
  )
  assertthat::assert_that(
    is.numeric(df$pe_obs_p_upper),
    msg = "Column 'pe_obs_p_upper' of 'df' must be numeric"
  )
  assertthat::assert_that(
    is.numeric(df$pe_alt_obs_p_upper),
    msg = "Column 'pe_alt_obs_p_upper' of 'df' must be numeric"
  )
  assertthat::assert_that(
    is.numeric(df$rpe_obs_p_upper),
    msg = "Column 'rpe_obs_p_upper' of 'df' must be numeric"
  )
  assertthat::assert_that(
    is.numeric(df$rpe_obs_p_lower),
    msg = "Column 'rpe_obs_p_lower' of 'df' must be numeric"
  )

  dplyr::mutate(
    df,
    # Categorize endemism by CANAPE scheme
    # (here, PE_orig = pe_obs_p, PE_alt = pe_alt_obs_p, and RPE = rpe_obs_p)
    #
    # 1)    If either PE_orig or PE_alt are significantly high then we look for palaeo or neo endemism
    #   a)    If RPE is significantly high then we have palaeo-endemism
    #         (PE_orig is consistently higher than PE_alt across the random realisations)
    #   b)    Else if RPE is significantly low then we have neo-endemism
    #         (PE_orig is consistently lower than PE_alt across the random realisations)
    #   c)    Else we have mixed age endemism in which case
    #        i)    If both PE_orig and PE_alt are highly significant (p<0.01) then we
    #              have super endemism (high in both palaeo and neo)
    #        ii)   Else we have mixed (some mixture of palaeo, neo and non endemic)
    # 2)    Else if neither PE_orig or PE_alt are significantly high then we have a non-endemic cell
    #' @srrstats {G3.0} use appropriate tolerances for approximate equality (see utils.R)
    endem_type = dplyr::case_when(
      is.na(pe_obs_p_upper) | is.na(pe_alt_obs_p_upper) | is.na(rpe_obs_p_upper) | is.na(rpe_obs_p_lower) ~ NA_character_,
      (pe_obs_p_upper %greater% 0.95 | pe_alt_obs_p_upper %greater% 0.95) & rpe_obs_p_upper %greater% 0.975 ~ "paleo",
      (pe_obs_p_upper %greater% 0.95 | pe_alt_obs_p_upper %greater% 0.95) & rpe_obs_p_lower %greater% 0.975 ~ "neo",
      pe_obs_p_upper %greater% 0.99 & pe_alt_obs_p_upper %greater% 0.99 ~ "super",
      pe_obs_p_upper %greater% 0.95 | pe_alt_obs_p_upper %greater% 0.95 ~ "mixed",
      TRUE ~ "not significant"
    )
  )
}
